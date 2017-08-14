package metascala.rt

import collection.mutable
import metascala._
import metascala.imm
import metascala.util.{Constants, Ref}

object Arr{
  trait ClsTable {
    def indexOfType(tpe: imm.Type.Cls): Int
    def typeAtIndex(i: Int): imm.Type
  }

  trait VMInterface {
    def heap: Heap
    def ClsTable: ClsTable
  }


  def unapply(x: Int)(implicit vm: VMInterface): Option[Arr] = Some(new Arr(x))


  implicit def unwrap2(x: Arr): Ref = x.address

  def packType(tpe: imm.Type.Arr)(implicit vm: VMInterface): Int = {
    var nesting = 0
    var current: imm.Type = tpe
    while ({
      current match {
        case arr: imm.Type.Arr =>
          current = arr.innerType
          nesting += 1
          true
        case _ => false
      }
    }){}

    val innerPacked = current match{
      // We pack primitives into the integer separately, since arrays can
      // contain primitives but primitives do not fit in the class table.
      case p: imm.Type.Prim[_] => p.index
      case cls: imm.Type.Cls => vm.ClsTable.indexOfType(cls) + imm.Type.Prim.indexed.length
    }
    ((nesting & 0xff) << 24) + innerPacked
  }

  def unpackType(tpeInfo: Int, inners: Int = 0)(implicit vm: VMInterface): imm.Type = {

    val n = tpeInfo & 0x00ffffff
    val innermostType =
      if (n < imm.Type.Prim.indexed.length) imm.Type.Prim.indexed(n)
      else vm.ClsTable.typeAtIndex(n - imm.Type.Prim.indexed.length)

    var nesting = tpeInfo >> 24
    var current: imm.Type = innermostType
    while (nesting > inners){
      current = imm.Type.Arr(current)
      nesting -= 1
    }

    current
  }
}

/**
  * Can be treated as a mutable sequence of Ints; this representation is
  * completely unaware of differently sized contents.
  */
class Arr(val address: Ref)(implicit vm: Arr.VMInterface) extends mutable.Seq[Int]{
  /**
    * Layout
    * ------
    * 0 [Array Nesting 32-24, Class Index 24-0]
    * 1 Length
    * 2 Field0
    * 3 Field1
    * ...
    * N FieldN-2
    *
    * Note that we pack the type of the array into the first 32-bit Int of the
    * array header. 8 bits are used for the array-nesting, while 24 bits are
    * used for the index into the classes table. This works because any runtime
    * type can be represented by a tuple of (arrayNesting, innermostType) since
    * any wrapping array types must only be in the outer layers.
    *
    * This lets us support arrays nested up to 256 layers deep and 16777216
    * unique classes in the JVM, and we can easily allocate more bits to it if
    * we wanted to support more.
    */


  /**
    * The type the array contains
    */
  def innerType = Arr.unpackType(vm.heap(address()), 1)

  /**
    * The type of the entire array
    */
  def tpe = Arr.unpackType(vm.heap(address()), 0)

  /**
    * Length of the array, in Ints or Longs, from the POV of interpreted code
    */
  def arrayLength = vm.heap(address() + 1)

  /**
    * Total size of the array on the heap
    */
  def heapSize = Constants.arrayHeaderSize + length

  /**
    * Length of the read-writable part of the array, in Ints.
    */
  def length = vm.heap(address() + 1) * innerType.size
  def apply(index: scala.Int) = vm.heap(address() + index + Constants.arrayHeaderSize)
  def update(index: scala.Int, value: Int) = vm.heap(address() + index + Constants.arrayHeaderSize) = value

  override def toString = ""+vm.heap.memory.slice(address(), address() + heapSize).toList

  def iterator: Iterator[Int] = vm.heap.memory.view(address() + Constants.arrayHeaderSize, address() + heapSize).iterator
}
