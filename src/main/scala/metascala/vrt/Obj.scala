package metascala
package vrt

import collection.mutable
import metascala._
import rt.Var

import reflect.ClassTag
import scala.Some

import metascala.imm


trait Ref{
  def tpe: imm.Type.Ref
  def longVal: scala.Long
}
object Obj{
  def apply(clsName: String, initMembers: (String, vrt.Val)*)(implicit vm: VM): vrt.Obj = {
    Obj.make(vm.ClsTable(imm.Type.Cls(clsName)), initMembers: _*)
  }
  def make(cls: rt.Cls, initMembers: (String, vrt.Val)*)(implicit vm: VM): vrt.Obj = {
    val address = vm.Heap.allocate(1 + cls.fieldList.length)
    vm.Heap(address) = cls.index
    val obj = new Obj(address)
    for ((s, v) <- initMembers){
      obj(imm.Type.Cls.read(cls.name), s) = v
    }
    obj
  }
  def unapply(x: Any) = x match{
    case x: Obj => Some((x.cls.name, x.members))
    case _ => None
  }
}
class Obj(address: scala.Int)
         (implicit vm: VM) extends StackVal with Cat1 with Ref{

  import vm._
  def longVal = address


  def cls: rt.Cls = vm.ClsTable.clsIndex(vm.Heap(address).toInt)

  object members{
    def apply(n: scala.Int): vrt.Val = {
      val longVal = vm.Heap(address)
      cls.fieldList(n).desc match{
        case imm.Type.Prim(c) =>
          val info = imm.Type.Prim.Info.charMap(c)
          info.fromLong(longVal)
        case imm.Type.Cls(name) => new vrt.Obj(n)
        case _: imm.Type.Arr =>
      }

    }

    def update(n: scala.Int, v: vrt.Val): Unit = {
      vm.Heap(address + n) = v.longVal
    }
  }

  def tpe = cls.clsData.tpe

  def apply(owner: imm.Type.Cls, name: String): vrt.Val = {
    members(owner.fieldList.lastIndexWhere(_.name == name))
  }

  def update(owner: imm.Type.Cls, name: String, value: vrt.Val) = {
    members(owner.fieldList.lastIndexWhere(_.name == name)) = value
  }


  override def toString = {
    s"vrt.Obj(${cls.name})"
  }
}

trait Arr[T] extends StackVal with Cat1 with Ref{
  def innerType: imm.Type
  def apply(index: scala.Int): vrt.Val
  def update(index: scala.Int, value: T): Unit
  def longVal = 0
  def length: Int
  def view: Array[T]
}

object Arr{

  val arrayTypeCache = mutable.Buffer.empty[imm.Type]
  object Obj{
    class TypeX[T](val t: imm.Type)

    def apply(t: imm.Type.Ref, n: scala.Int)(implicit vm: VM): Arr.Obj = {
      Arr.Obj(t, Array.fill[vrt.Val](n)(t.default))
    }
    def apply(innerType: imm.Type.Ref, backing: Array[vrt.Val])(implicit vm: VM): Arr.Obj = {
      val address = vm.Heap.allocate(1 + backing.length)
      vm.Heap(address) = arrayTypeCache.length
      arrayTypeCache.append(innerType)
      vm.Heap(address + 1) = backing.length
      backing.map(_.longVal).copyToArray(vm.Heap.memory, address.toInt + 2)
      new Obj(address)
    }
  }
  class Obj(address: scala.Int)(implicit vm: VM) extends Arr[vrt.Ref]{
    def innerType = arrayTypeCache(address.toInt)
    val tpe = imm.Type.Arr(innerType)
    def length = vm.Heap(address + 1).toInt
    def apply(index: scala.Int) = new vrt.Obj(vm.Heap(address + index + 2).toInt)
    def update(index: scala.Int, value: vrt.Ref) = vm.Heap(address + index + 2) = value.longVal
    override def toString = s"vrt.Arr.Obj(${innerType.getClass} ${innerType.unparse}: ${view.fold("")(_+", "+_)})"

    def view = vm.Heap.memory.slice(address + 1, address + length + 1).map(x => new vrt.Obj(x.toInt)).toArray
  }
  object Prim{
    def apply[T](n: scala.Int)(implicit ct: ClassTag[T], info: imm.Type.Prim.Info[T], vm: VM): vrt.Arr.Prim[T] = {
      Arr.Prim(new Array[T](n))
    }
    def apply[T](backing: Array[T])(implicit vm: VM, info: imm.Type.Prim.Info[T], ct: ClassTag[T]): vrt.Arr.Prim[T] = {
      val address = vm.Heap.allocate(2 + backing.length)
      vm.Heap(address + 1) = backing.length
      backing.map(info.toLong).copyToArray(vm.Heap.memory, address + 2)
      vrt.Arr.Prim(address)
    }
    def unapply(s: vrt.Val) = s match{
      case x: vrt.Arr.Prim[_] => Some(x.view)
      case _ => None
    }

  }
  class Prim[T](val address: scala.Int)(implicit vm: VM, info: imm.Type.Prim.Info[T], ct: ClassTag[T]) extends Arr[T]{

    lazy val innerType: imm.Type.Prim = imm.Type.Prim.Info()
    val tpe = imm.Type.Arr(innerType)
    def length = vm.Heap(address + 1).toInt
    def charClass = implicitly[imm.Type.Prim.Info[T]]
    def apply(index: scala.Int) = charClass.fromLong(vm.Heap.memory(address + index + 2))
    def update(index: scala.Int, value: T) = vm.Heap.memory(address + index + 2) = charClass.toLong(value)
    override def toString = s"vrt.PrimArr(${innerType.unparse}: ${view.fold("")(_+", "+_)})"

    def view = vm.Heap.memory.slice(address + 1, address + length + 1).map(charClass.longToRaw).toArray
  }
}






