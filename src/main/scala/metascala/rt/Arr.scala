package metascala.rt

import collection.mutable
import metascala._
import metascala.imm
import scala.Some


object Arr{


  /**
    * Allocates and returns an array of the specified type, with `n` elements.
    * This is multiplied with the size of the type being allocated when
    * calculating the total amount of memory benig allocated
    */
  def alloc(t: imm.Type, n: scala.Int)(implicit registrar: Registrar): Arr = {
    rt.Arr.alloc(t, Array.fill[Int](n * t.size)(0).map(x => new Ref.ManualRef(x): Ref))
  }
  def alloc(innerType: imm.Type, backing0: TraversableOnce[Ref])(implicit registrar: Registrar): Arr = {
    implicit val vm = registrar.vm
    val backing = backing0.toArray
    val address = vm.heap.allocate(Constants.arrayHeaderSize+ backing.length)(registrar.apply)
    //    println("Allocating Array[" + innerType.name + "] at " + address)
    vm.heap(address()) = vm.arrayTypeCache.length

    vm.arrayTypeCache.append(innerType)
    vm.heap(address() + 1) = backing.length / innerType.size
    backing.map(_()).copyToArray(vm.heap.memory, address() + Constants.arrayHeaderSize)

    new Arr(address)
  }
  def unapply(x: Int)(implicit vm: VMInterface_2): Option[Arr] = Some(new Arr(x))

  implicit def unwrap1(x: Arr): Int = x.address.apply()
  implicit def unwrap2(x: Arr): Ref = x.address
}

/**
  * Can be treated as a mutable sequence of Ints; this representation is
  * completely unaware of differently sized contents.
  */
class Arr(val address: Ref)(implicit vm: VMInterface_2) extends mutable.Seq[Int]{
  /**
    * Layout
    * ------
    * 0 Class Index
    * 1 Length
    * 2 Field0
    * 3 Field1
    * ...
    * N FieldN-2
    */

  /**
    * The type the array contains
    */
  def innerType = vm.arrayTypeCache(vm.heap(address()))

  /**
    * The type of the entire array
    */
  def tpe = imm.Type.Arr(innerType)

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
  def update(index: scala.Int, value: Val) = vm.heap(address() + index + Constants.arrayHeaderSize) = value

  override def toString = ""+vm.heap.memory.slice(address(), address() + heapSize).toList

  def iterator: Iterator[Int] = vm.heap.memory.view(address() + Constants.arrayHeaderSize, address() + heapSize).iterator
}
