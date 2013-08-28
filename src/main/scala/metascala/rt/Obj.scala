package metascala.rt

import collection.mutable
import metascala._
import metascala.imm
import scala.Some


object Obj{
  val headerSize = 2
  def allocate(cls: rt.Cls, initMembers: (String, Ref)*)(implicit registrar: Registrar): Obj = {
    implicit val vm = registrar.vm
    val address = vm.heap.allocate(headerSize + cls.fieldList.length)
//    println("Allocating " + cls.name + " at " + address)
    vm.heap(address()) = -cls.index
    vm.heap(address() + 1) = cls.fieldList.length
    val obj = new Obj(address)
    for ((s, v) <- initMembers){
      obj(s) = v()
    }
    obj
  }

  def unapply(x: Val)(implicit vm: VM) = new Obj(x)
}

class Obj(val address: Ref)
         (implicit vm: VM) {
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
  import vm._

  def cls: rt.Cls = vm.ClsTable.clsIndex(-vm.heap(address()).toInt)

  object members extends mutable.Seq[Val]{
    def apply(n: Int): Val = {
      vm.heap(address() + n + Obj.headerSize)
    }

    def update(n: Int, v: Val): Unit = {
      vm.heap(address() + n + Obj.headerSize) = v
    }

    def length = cls.fieldList.length

    def iterator: Iterator[Val] = new Iterator[Val]{
      var index = 0
      def hasNext = index < members.length
      def next() = {
        val x = vm.heap(address() + index + Obj.headerSize)
        index += 1
        x
      }
    }
  }

  def tpe = cls.tpe

  def heapSize = cls.heapSize

  def apply(name: String): Val = {
    members(tpe.fieldList.lastIndexWhere(_.name == name))
  }

  def update(name: String, value: Val) = {

    val index = tpe.fieldList.lastIndexWhere(_.name == name)
    members(index + 1 - tpe.fieldList(index).desc.size) = value
  }

  def view = address + " " + vm.heap.memory.slice(address(), address() + heapSize).toList

  override def toString = {
    s"vrt.Obj(${cls.name})"
  }
}


object Arr{
  val headerSize = 2

  /**
   * Allocates and returns an array of the specified type, with `n` elements.
   * This is multiplied with the size of the type being allocated when
   * calculating the total amount of memory benig allocated
   */
  def allocate(t: imm.Type, n: scala.Int)(implicit registrar: Registrar): Arr = {
    rt.Arr.allocate(t, Array.fill[Int](n * t.size)(0).map(x => new ManualRef(x): Ref))
  }
  def allocate(innerType: imm.Type, backing: Array[Ref])(implicit registrar: Registrar): Arr = {
    implicit val vm = registrar.vm
    val address = vm.heap.allocate(Arr.headerSize + backing.length)
//    println("Allocating Array[" + innerType.name + "] at " + address)
    vm.heap(address()) = vm.arrayTypeCache.length

    vm.arrayTypeCache.append(innerType)
    vm.heap(address() + 1) = backing.length / innerType.size
    backing.map(_()).copyToArray(vm.heap.memory, address() + Arr.headerSize)

    new Arr(address)
  }
  def unapply(x: Int)(implicit vm: VM): Option[Arr] = Some(new Arr(x))
}

/**
 * Can be treated as a mutable sequence of Ints; this representation is
 * completely unaware of differently sized contents.
 */
class Arr(val address: Ref)(implicit vm: VM) extends mutable.Seq[Int]{
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
  def heapSize = Arr.headerSize + length

  /**
   * Length of the read-writable part of the array, in Ints.
   */
  def length = vm.heap(address() + 1) * innerType.size
  def apply(index: scala.Int) = vm.heap(address() + index + Arr.headerSize)
  def update(index: scala.Int, value: Val) = vm.heap(address() + index + Arr.headerSize) = value

  override def toString = ""+vm.heap.memory.slice(address(), address() + heapSize).toList

  def iterator: Iterator[Int] = vm.heap.memory.view(address() + Arr.headerSize, address() + heapSize).iterator
}