package metascala

import scala.collection.mutable
import imm.Type.Prim
import imm.Type.Prim._
import metascala.rt.{Arr, Obj}

object Virtualizer {
  lazy val unsafe = {
    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    field.get(null).asInstanceOf[sun.misc.Unsafe]
  }

  def popVirtual(tpe: imm.Type, src: () => Val, refs: mutable.Map[Int, Any] = mutable.Map.empty)(implicit vm: VM): Any = {

    val x = tpe match {
      case V => ()
      case p: imm.Type.Prim[_] => p.read(src)
      case _ => //reference type
        val address = src()

        if(address == 0) null
        else if (refs.contains(address)) refs(address)
        else tpe match{
          case imm.Type.Cls("java/lang/Object") | imm.Type.Arr(_) if address.isArr =>
            val tpe = vm.arrayTypeCache(vm.heap(address))

            val clsObj = forName(tpe.name.toDot)
            val newArr = java.lang.reflect.Array.newInstance(clsObj, address.arr.arrayLength)

            for(i <- 0 until address.arr.arrayLength){

              val cooked = tpe match{
                case p: imm.Type.Prim[_] => p.read(reader(vm.heap.memory, address + rt.Arr.headerSize + i * tpe.size))
                case x => popVirtual(tpe, reader(vm.heap.memory, address + rt.Arr.headerSize + i * tpe.size))
              }
              println("Setting " + i + " " + cooked)
              java.lang.reflect.Array.set(newArr, i, cooked)
            }

            newArr
          case t @ imm.Type.Cls(name)=>
            println("Popping " + address + name)
            val obj = unsafe.allocateInstance(Class.forName(address.obj.cls.name.toDot))
            refs += (address -> obj)
            var index = 0
            for(field <- address.obj.cls.fieldList.distinct){
              // workaround for http://bugs.sun.com/view_bug.do?bug_id=4763881
              if (field.name == "backtrace") index += 1 // just skip it
              else{
                val f = getAllFields(obj.getClass).find(_.getName == field.name).get
                f.setAccessible(true)

                f.set(obj, popVirtual(field.desc, reader(vm.heap.memory, address + rt.Obj.headerSize + index), refs))
                index += field.desc.size
              }
            }
            obj

        }
    }

    x
  }

  def pushVirtual(thing: Any)(implicit registrar: Registrar): Seq[Int] = {
    val tmp = new mutable.Stack[Int]()
    pushVirtual(thing, tmp.push(_))
    tmp.reverse
  }

  def pushVirtual(thing: Any, out: Val => Unit)(implicit registrar: Registrar): Unit = {
    implicit val vm = registrar.vm
    thing match {
      case null => out(0)
      case b: Boolean => Z.write(b, out)
      case b: Byte    => B.write(b, out)
      case b: Char    => C.write(b, out)
      case b: Short   => S.write(b, out)
      case b: Int     => I.write(b, out)
      case b: Float   => F.write(b, out)
      case b: Long    => J.write(b, out)
      case b: Double  => D.write(b, out)
      case b: Array[_] =>
        val arr =
          rt.Arr.allocate(imm.Type.Arr.read(b.getClass.getName.replace('.', '/')).innerType,
            b.flatMap(pushVirtual)
          )

        out(arr.address)
      case b: Any =>
        var index = 0
        val contents = mutable.Buffer.empty[Int]
        for(field <- vm.ClsTable(imm.Type.Cls(b.getClass.getName.toSlash)).fieldList.distinct) yield {
          val f = b.getClass.getDeclaredField(field.name)
          f.setAccessible(true)
          pushVirtual(f.get(b), contents.append(_))
          index += field.desc.size
        }
        val obj = rt.Obj.allocate(b.getClass.getName.toSlash)
        contents.map(writer(vm.heap.memory, obj.address + rt.Obj.headerSize))
        out(obj.address)
    }
  }
}
