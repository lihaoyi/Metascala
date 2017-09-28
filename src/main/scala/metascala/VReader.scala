package metascala

import imm.Type.Prim._
import metascala.util.{Constants, Util}

import scala.reflect.ClassTag

trait VReader[T]{
  def readAny(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]): T
  def size: Int
}
object VReader{

  def obj[T >: Null: ObjectReader] = implicitly[ObjectReader[T]]
  def apply[T: VReader] = implicitly[VReader[T]]
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
    def readAnyRefValue(heapAddress: Int, heap: IndexedSeq[Int]): T
    def readAnyRef(heapAddress: Int, heap: IndexedSeq[Int]): T = {
      if (heapAddress == 0) null
      else readAnyRefValue(heapAddress, heap)
    }
    def readAny(address: Int, locals: IndexedSeq[Int], heap: IndexedSeq[Int]) = {
      readAnyRef(locals(address), heap)
    }
  }
  implicit def ArrayReader[T: VReader: ClassTag] = new ObjectReader[Array[T]] {
    def readAnyRefValue(heapAddress: Int, heap: IndexedSeq[I]) = {

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
    def readAnyRefValue(heapAddress: Int, heap: IndexedSeq[I]) = {
      val chars = ArrayReader[Char].readAny(heapAddress + Constants.objectHeaderSize, heap, heap)
      new String(chars)
    }
  }

}