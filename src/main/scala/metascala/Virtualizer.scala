package metascala
import scala.collection.mutable
import imm.Type.Prim._
import metascala.util.{Constants, Ref, Util}

object Virtualizer {
  def toVirtObj(x: Any)(implicit registrar: rt.Allocator) = {
    Virtualizer.pushVirtual(x).apply(0)
  }

  def pushVirtual(thing: Any)(implicit registrar: rt.Allocator): Seq[Int] = {
    val tmp = new mutable.ArrayBuffer[Int]()
    pushVirtual(thing, tmp.append(_))
    tmp
  }

  def pushVirtual(thing: Any, out: Int => Unit)(implicit registrar: rt.Allocator): Unit = {

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

        val tpe = imm.Type.Arr.read(b.getClass.getName.replace('.', '/')).innerType

        val arr = registrar.newArr(tpe, b.length)
        var i = 0
        for(v <- b){
          pushVirtual(v, { x =>
            vm.heap(arr.address() + Constants.arrayHeaderSize + i) = x
            i += 1
          })
        }
        out(arr.address())
      case b: Any =>
        var index = 0
        val contents = mutable.Buffer.empty[Ref]
        val decFields = b.getClass.getDeclaredFields
        for(field <- vm.clsTable(imm.Type.Cls.apply(b.getClass.getName)).fieldList0) yield {
          val f = decFields.find(_.getName == field.name).get
          f.setAccessible(true)
          pushVirtual(f.get(b), x =>
            contents.append(Ref.Raw(x))
          )
          index += field.desc.size
        }

        val obj = registrar.newObj(b.getClass.getName)
        contents.map(_()).map(Util.writer(vm.heap.memory, obj.address() + Constants.objectHeaderSize))
        out(obj.address())
    }
  }
}
