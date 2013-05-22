package metascala
package vrt

import collection.mutable
import metascala._
import rt.Var

import reflect.ClassTag
import scala.Some

import metascala.imm



object Obj{
  def allocate(cls: rt.Cls, initMembers: (String, Val)*)(implicit vm: VM): vrt.Obj = {
    val address = vm.Heap.allocate(2 + cls.fieldList.length)
    vm.Heap(address) = -cls.index
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
   * 1 Field0
   * 2 Field1
   *
   * ...
   * N FieldN-1
   */
  import vm._

  def cls: rt.Cls = vm.ClsTable.clsIndex(-vm.Heap(address).toInt)

  object members extends mutable.Seq[Val]{
    def apply(n: Int): Val = {
      vm.Heap(address + n + 2)
    }

    def update(n: Int, v: Val): Unit = {
      vm.Heap(address + n + 2) = v
    }

    def length = cls.fieldList.length

    def iterator: Iterator[Val] = new Iterator[Val]{
      var index = 0
      def hasNext = index < members.length
      def next() = {
        val x = vm.Heap(address + index + 2)
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

  def view = address + " " + vm.Heap.memory.slice(address, address + cls.fieldList.length + 2).toList

  override def toString = {
    s"vrt.Obj(${cls.name} + )"
  }
}


object Arr{

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
    val address = vm.Heap.allocate(2 + backing.length)
    vm.Heap(address) = arrayTypeCache.length

    arrayTypeCache.append(innerType)
    vm.Heap(address + 1) = backing.length / innerType.size
    backing.copyToArray(vm.Heap.memory, address + 2)

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
  def innerType = Arr.arrayTypeCache(vm.Heap(address))
  def tpe = imm.Type.Arr(innerType)
  def length = vm.Heap(address + 1)
  def apply(index: scala.Int) = vm.Heap(address + index + 2)
  def update(index: scala.Int, value: Val) = vm.Heap(address + index + 2) = value

  override def toString = ""+vm.Heap.memory.slice(address, address + length * innerType.size + 2).toList

  def iterator: Iterator[Int] = vm.Heap.memory.view(address, address + length * innerType.size + 2).iterator
}







