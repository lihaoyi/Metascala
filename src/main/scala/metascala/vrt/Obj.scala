package metascala
package vrt

import collection.mutable
import metascala._
import rt.Var

import reflect.ClassTag
import scala.Some

import metascala.imm



object Obj{
  val headerSize = 2
  def allocate(cls: rt.Cls, initMembers: (String, Val)*)(implicit vm: VM): vrt.Obj = {
    val address = vm.heap.allocate(headerSize + cls.fieldList.length)
//    println("Allocating " + cls.name + " at " + address)
    vm.heap(address) = -cls.index
    val obj = new Obj(address)
    for ((s, v) <- initMembers){
      obj(s) = v
    }
    obj
  }

  def unapply(x: Val)(implicit vm: VM) = new Obj(x)
}

class Obj(val address: Val)
         (implicit vm: VM) {
  /**
   * Layout
   * ------
   * 0 Class Index
   * 1 -
   * 2 Field0
   * 3 Field1
   * ...
   * N FieldN-2
   */
  import vm._

  def cls: rt.Cls = vm.ClsTable.clsIndex(-vm.heap(address).toInt)

  object members extends mutable.Seq[Val]{
    def apply(n: Int): Val = {

      vm.heap(address + n + Obj.headerSize)
    }

    def update(n: Int, v: Val): Unit = {
      vm.heap(address + n + Obj.headerSize) = v
    }

    def length = cls.fieldList.length

    def iterator: Iterator[Val] = new Iterator[Val]{
      var index = 0
      def hasNext = index < members.length
      def next() = {
        val x = vm.heap(address + index + Obj.headerSize)
        index += 1
        x
      }
    }
  }

  def tpe = cls.clsData.tpe

  def apply(name: String): Val = {
    members(tpe.fieldList.lastIndexWhere(_.name == name))
  }

  def update(name: String, value: Val) = {

    val index = tpe.fieldList.lastIndexWhere(_.name == name)
    members(index + 1 - tpe.fieldList(index).desc.size) = value
  }

  def view = address + " " + vm.heap.memory.slice(address, address + cls.fieldList.length + Obj.headerSize).toList

  override def toString = {
    s"vrt.Obj(${cls.name} + )"
  }
}


object Arr{
  val headerSize = 2
  val arrayTypeCache = mutable.Buffer.empty[imm.Type]

  /**
   * Allocates and returns an array of the specified type, with `n` elements.
   * This is multiplied with the size of the type being allocated when
   * calculating the total amount of memory benig allocated
   */
  def allocate(t: imm.Type, n: scala.Int)(implicit vm: VM): Arr = {
    vrt.Arr.allocate(t, Array.fill[Int](n * t.size)(0))
  }
  def allocate(innerType: imm.Type, backing: Array[Int])(implicit vm: VM): Arr = {
    val address = vm.heap.allocate(Arr.headerSize + backing.length)
//    println("Allocating Array[" + innerType.name + "] at " + address)
    vm.heap(address) = arrayTypeCache.length

    arrayTypeCache.append(innerType)
    vm.heap(address + 1) = backing.length / innerType.size
    backing.copyToArray(vm.heap.memory, address + Arr.headerSize)

    new Arr(address)
  }
  def unapply(x: Int)(implicit vm: VM): Option[Arr] = Some(new Arr(x))
}
class Arr(val address: scala.Int)(implicit vm: VM) extends mutable.Seq[Int]{
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
  def longVal = address
  def innerType = Arr.arrayTypeCache(vm.heap(address))
  def tpe = imm.Type.Arr(innerType)
  def length = vm.heap(address + 1)
  def apply(index: scala.Int) = vm.heap(address + index + Arr.headerSize)
  def update(index: scala.Int, value: Val) = vm.heap(address + index + Arr.headerSize) = value

  override def toString = ""+vm.heap.memory.slice(address, address + length * innerType.size + Arr.headerSize).toList

  def iterator: Iterator[Int] = vm.heap.memory.view(address, address + length * innerType.size + Arr.headerSize).iterator
}







