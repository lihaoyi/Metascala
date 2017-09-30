package metascala.heap

import metascala.imm
import metascala.imm.Type.Prim.{B, C, D, F, I, J, S, Z}
import metascala.util.{Constants, Util}

import scala.reflect.ClassTag

/**
  * Extracts values from the Metascala VM's heap into objects in the outside
  * world. Implement as a typeclass rather than using reflection/unsafe for
  * security reasons: this ensures that the VM-internal code is unable to
  * instantiate arbitrary objects in the outside world.
  */
trait HeapReader[T]{
  def readAny(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]): T
  def size: Int
}
object HeapReader{

  def obj[T >: Null: ObjectReader] = implicitly[ObjectReader[T]]
  def apply[T: HeapReader] = implicitly[HeapReader[T]]
  class PrimReader[T](prim: imm.Type.Prim[T]) extends HeapReader[T]{
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
  implicit object NullReader extends HeapReader[Null]{
    def readAny(address: I, locals: IndexedSeq[I], heap: IndexedSeq[I]) = {
      assert(locals(address) == 0)
      null
    }
    def size = 1
  }
  implicit object UnitReader extends HeapReader[Unit]{
    def readAny(address: I, locals: IndexedSeq[I], heap: IndexedSeq[I]) = ()
    def size = 0
  }

  abstract class ObjectReader[T >: Null] extends HeapReader[T]{
    def size = 1
    def readAnyRefValue(heapAddress: Int, heap: IndexedSeq[Int]): T
    def readAnyRef(heapAddress: Int, heap: IndexedSeq[Int]): T = {
      if (heapAddress == 0) null
      else readAnyRefValue(heapAddress, heap)
    }
    def readAny(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]) = {
      readAnyRef(locals(address), heap)
    }
  }
  implicit def ArrayReader[T: HeapReader: ClassTag] = new ObjectReader[Array[T]] {
    def readAnyRefValue(heapAddress: Int, heap: IndexedSeq[I]) = {

      val length = heap(heapAddress + 1)
      val arr = new Array[T](length)
      var offset = heapAddress + Constants.arrayHeaderSize
      val elemReader = implicitly[HeapReader[T]]
      for(i <- 0 until length){
        val v = elemReader.readAny(offset, heap, heap)
        arr(i) = v
        offset += elemReader.size
      }
      arr
    }
  }
  implicit object StringReader extends ObjectReader[String] {
    def readAnyRefValue(heapAddress: Int, heap: IndexedSeq[I]) = {
      val chars = ArrayReader[Char].readAny(heapAddress + Constants.objectHeaderSize, heap, heap)
      new String(chars)
    }
  }
  implicit object StackTraceElementReader extends ObjectReader[StackTraceElement] {
    def readAnyRefValue(heapAddress: Int, heap: IndexedSeq[I]) = {
      new StackTraceElement(
        StringReader.readAny(heapAddress + Constants.objectHeaderSize, heap, heap),
        StringReader.readAny(heapAddress + Constants.objectHeaderSize + 1, heap, heap),
        StringReader.readAny(heapAddress + Constants.objectHeaderSize + 2, heap, heap),
        IntReader.readAny(heapAddress + Constants.objectHeaderSize + 3, heap, heap)
      )
    }
  }



}