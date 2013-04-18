package metascala
package vrt

import collection.mutable
import metascala._
import rt.Var

import reflect.ClassTag
import scala.Some

import metascala.imm



object Obj{
  def allocate(clsName: String, initMembers: (String, Val)*)(implicit vm: VM): vrt.Obj = {
    Obj.allocate(vm.ClsTable(imm.Type.Cls(clsName)), initMembers: _*)
  }
  def allocate(cls: rt.Cls, initMembers: (String, Val)*)(implicit vm: VM): vrt.Obj = {
    val address = vm.Heap.allocate(1 + cls.fieldList.length)
    println("Allocated Obj at " + address)
    vm.Heap(address) = -cls.index
    val obj = new Obj(address)
    for ((s, v) <- initMembers){
      obj(imm.Type.Cls.read(cls.name), s) = v
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
      println("Updating ! " + (address + n + 2))
      vm.Heap(address + n + 2) = v
    }

    def length = cls.fieldList.length

    def iterator: Iterator[Val] = new Iterator[Val]{
      var index = 0
      def hasNext = index < members.length
      def next() = vm.Heap(address + index + 2)
    }
  }

  def tpe = cls.clsData.tpe

  def apply(owner: imm.Type.Cls, name: String): Val = {
    members(owner.fieldList.lastIndexWhere(_.name == name))
  }

  def update(owner: imm.Type.Cls, name: String, value: Val) = {
    members(owner.fieldList.lastIndexWhere(_.name == name)) = value
  }

  override def toString = {
    s"vrt.Obj(${cls.name} + )"
  }
}


object Arr{

  val arrayTypeCache = mutable.Buffer.empty[imm.Type]


  def allocate(t: imm.Type, n: scala.Int)(implicit vm: VM): Arr = {
    vrt.Arr.allocate(t, Array.fill[Int](n)(0))
  }
  def allocate(innerType: imm.Type, backing: Array[Int])(implicit vm: VM): Arr = {
    val address = vm.Heap.allocate(2 + backing.length)
    vm.Heap(address) = arrayTypeCache.length
    println("Allocated Arr at " + address)
    arrayTypeCache.append(innerType)
    vm.Heap(address + 1) = backing.length
    backing.copyToArray(vm.Heap.memory, address + 2)
    (new Arr(address))

  }
  def unapply(x: Int)(implicit vm: VM): Option[Arr] = Some(new Arr(x))
}
class Arr(val address: scala.Int)(implicit vm: VM) {
  /**
   * Layout
   * ------
   * 0 Class Index
   * 1 Field0
   * 2 Field1
   * ...
   * N FieldN-1
   */
  def longVal = address
  def innerType = Arr.arrayTypeCache(vm.Heap(address))
  def tpe = imm.Type.Arr(innerType)
  def length = vm.Heap(address + 1).toInt
  def apply(index: scala.Int) = vm.Heap(address + index + 2)
  def update(index: scala.Int, value: Val) = vm.Heap(address + index + 2) = value
  override def toString = s"vrt.Arr(${innerType.getClass})"

  def view = vm.Heap.memory.slice(address + 1, address + length + 1).toArray
}







