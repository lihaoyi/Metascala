package metascala
import scala.collection.mutable
import imm.Type.Prim._
import metascala.imm.Type.Prim
import metascala.natives.Bindings
import metascala.util.{Constants, Ref, Util}

import scala.reflect.ClassTag

trait VReader[T]{
  def readAny(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]): T
  def size: Int
}
object VReader{

  class PrimReader[T](prim: imm.Type.Prim[T]) extends VReader[T]{
    def size = prim.size
    def readAny(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]) = {
      prim.read(Util.reader(locals, address))
    }
  }
  implicit object BooleanReader extends PrimReader(Z)
  implicit object ByteReader extends PrimReader(B)
  implicit object ShortReader extends PrimReader(S)
  implicit object CharReader extends PrimReader(C)
  implicit object IntReader extends PrimReader(I)
  implicit object FloatReader extends PrimReader(F)
  implicit object LongReader extends PrimReader(J)
  implicit object DoubleReader extends PrimReader(D)
  implicit object NullReader extends VReader[Null]{
    def readAny(address: I, locals: IndexedSeq[I], heap: IndexedSeq[I]) = {
      assert(locals(address) == 0)
      null
    }
    def size = 1
  }

  abstract class ObjectReader[T >: Null] extends VReader[T]{
    def size = 1
    def readAnyRef(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]): T
    def readAny(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]) = {
      if (locals(address) == 0) null
      else readAnyRef(address, locals, heap)

    }
  }
  implicit def ArrayReader[T: VReader: ClassTag] = new ObjectReader[Array[T]] {
    def readAnyRef(address: Int, locals: IndexedSeq[I], heap: IndexedSeq[I]) = {
      val heapAddress = locals(address)
      val length = heap(heapAddress + 1)
      val arr = new Array[T](length)
      var offset = heapAddress + Constants.arrayHeaderSize
      val elemReader = implicitly[VReader[T]]
      for(i <- 0 until length){
        val v = elemReader.readAny(offset, heap, heap)
        arr(i) = v
        offset += elemReader.size
      }
      arr
    }
  }
  implicit object StringReader extends ObjectReader[String] {
    def readAnyRef(address: Int, locals: IndexedSeq[I], heap: IndexedSeq[I]) = {
      val heapAddress = locals(address)
      val chars = ArrayReader[Char].readAny(heapAddress + Constants.objectHeaderSize, heap, heap)
      new String(chars)
    }
  }

}
object Virtualizer {
  def toRealObjSafe[T: VReader](v: Int)(implicit vm: Bindings.Interface) = {
    implicitly[VReader[T]].readAny(v, vm.heap.memory, vm.heap.memory)
  }
  def toRealObj[T](v: Int)(implicit vm: Bindings.Interface, ct: ClassTag[T]) = {
    Virtualizer.popVirtual(ct.runtimeClass.getName.replace('.', '/'), () => v)
      .asInstanceOf[T]
  }
  def toVirtObj(x: Any)(implicit registrar: rt.Allocator) = {
    Virtualizer.pushVirtual(x).apply(0)
  }

  lazy val unsafe = {
    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    val f = field.get(null)
    val g = f.asInstanceOf[sun.misc.Unsafe]
    g
  }

  def popVirtual(tpe: imm.Type,
                 src: () => Int,
                 refs: mutable.Map[Int, Any] = mutable.Map.empty)
                (implicit vm: Bindings.Interface): Any = {
    val x = tpe match {
      case V => ()
      case p: imm.Type.Prim[_] => p.read(src)
      case _ => //reference type
        val address = src()
        if(address == 0) null
        else if (refs.contains(address)) refs(address)
        else tpe match{
          case imm.Type.Cls("java.lang.Object") | imm.Type.Arr(_) if vm.isArr(address) =>

            val tpe = vm.arr(address).innerType

            val clsObj = tpe match{
              case v: imm.Type.Prim[_] => v.primClass
              case _ => Class.forName(tpe.javaName)
            }

            val newArr = java.lang.reflect.Array.newInstance(clsObj, vm.arr(address).arrayLength)

            for(i <- 0 until vm.arr(address).arrayLength){

              val cooked = tpe match{
                case p: imm.Type.Prim[_] => p.read(Util.reader(vm.heap.memory, address + Constants.arrayHeaderSize + i * tpe.size))
                case x => popVirtual(tpe, Util.reader(vm.heap.memory, address + Constants.arrayHeaderSize + i * tpe.size))
              }
              java.lang.reflect.Array.set(newArr, i, cooked)
            }

            newArr
          case t @ name=>
            val obj = unsafe.allocateInstance(Class.forName(vm.obj(address).cls.tpe.javaName))
            refs += (address -> obj)
            var index = 0
            for(field <- vm.obj(address).cls.fieldList0){
              // workaround for http://bugs.sun.com/view_bug.do?bug_id=4763881
              if (field.name == "backtrace") index += 1 // just skip it
              else{
                val f = getAllFields(obj.getClass).find(_.getName == field.name).get
                f.setAccessible(true)
                val popped = popVirtual(field.desc, Util.reader(vm.heap.memory, address + Constants.objectHeaderSize + index), refs)
                f.set(obj, popped )
                index += field.desc.size
              }
            }
            obj

        }
    }

    x
  }

  def getAllFields(cls: Class[_]): Seq[java.lang.reflect.Field] = {
    Option(cls.getSuperclass)
      .toSeq
      .flatMap(getAllFields)
      .++(cls.getDeclaredFields)
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
