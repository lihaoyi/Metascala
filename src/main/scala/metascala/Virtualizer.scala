package metascala

import scala.collection.mutable
import imm.Type.Prim
import imm.Type.Prim._
object Virtualizer {
  def popVirtual(tpe: imm.Type, src: () => Val, refs: mutable.Map[Int, Any] = mutable.Map.empty)(implicit vm: VM): Any = {
    tpe match {
      case V => ()
      case p: imm.Type.Prim[_] => p.read(src)
      case _ => //reference type
        val address = src()
        if(address == 0) null
        else if (refs.contains(address)) refs(address)
        else tpe match{
          case t @ imm.Type.Cls(name) =>
            val obj = vrt.unsafe.allocateInstance(Class.forName(address.obj.cls.name.toDot))
            refs += (address -> obj)
            var index = 0
            for(field <- address.obj.cls.fieldList.distinct){
              // workaround for http://bugs.sun.com/view_bug.do?bug_id=4763881
              if (field.name == "backtrace") index += 1 // just skip it
              else{
                val f = getAllFields(obj.getClass).find(_.getName == field.name).get
                f.setAccessible(true)
                f.set(obj, popVirtual(field.desc, reader(vm.Heap.memory, address + 2 + index), refs))
                index += field.desc.size
              }
            }
            obj
          case t @ imm.Type.Arr(tpe) =>

            val clsObj = forName(tpe.unparse.toDot)
            val newArr = java.lang.reflect.Array.newInstance(clsObj, address.arr.length)

            for(i <- 0 until address.arr.length){

              val cooked = tpe match{
                case p: imm.Type.Prim[_] => p.read(reader(vm.Heap.memory, address + 2 + i * tpe.size))
                case x => popVirtual(tpe, reader(vm.Heap.memory, address + 2 + i * tpe.size))
              }
              java.lang.reflect.Array.set(newArr, i, cooked)
            }
            newArr
        }
    }
  }

  def pushVirtual(thing: Any)(implicit vm: VM): Seq[Int] = {
    val tmp = new mutable.Stack[Int]()
    pushVirtual(thing, tmp.push(_))
    tmp.reverse
  }

  def pushVirtual(thing: Any, out: Val => Unit)(implicit vm: VM): Unit = {
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
        val arr = vrt.Arr.allocate(imm.Type.Arr.read(b.getClass.getName).innerType,
          b.flatMap(pushVirtual)
        )
        out(arr.address)
      case b: Any =>
        val obj = vrt.Obj.allocate(b.getClass.getName.toSlash)
        var index = 0
        for(field <- obj.cls.clsData.fields.filter(!_.static)){
          val f = b.getClass.getDeclaredField(field.name)
          f.setAccessible(true)
          pushVirtual(f.get(b), writer(vm.Heap.memory, obj.address + 2 + index))
          index += field.desc.size
        }

        out(obj.address)
    }
  }
}
