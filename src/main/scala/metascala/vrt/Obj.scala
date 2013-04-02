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
}
object Obj{
  def apply(clsName: String, initMembers: (String, vrt.Val)*)(implicit vm: VM) = {
    new Obj(vm.ClsTable(imm.Type.Cls(clsName)), initMembers: _*)
  }
  def unapply(x: Any) = x match{
    case x: Obj => Some((x.cls.name, x.members))
    case _ => None
  }
}
class Obj(val cls: rt.Cls, initMembers: (String, vrt.Val)*)
         (implicit vm: VM) extends StackVal with Cat1 with Ref{
  import vm._

  val members = cls.fieldList.map(x => (x, new Var(x.desc.default)))

  for ((s, v) <- initMembers){
    this(imm.Type.Cls.read(cls.name), s) = v
  }

  def tpe = cls.clsData.tpe

  def apply(owner: imm.Type.Cls, name: String): vrt.Val = {
    members(owner.fieldList.lastIndexWhere(_.name == name))._2()
  }

  def update(owner: imm.Type.Cls, name: String, value: vrt.Val) = {
    members(owner.fieldList.lastIndexWhere(_.name == name))._2() = value
  }


  override def toString = {
    s"vrt.Obj(${cls.name})"
  }
}

trait Arr extends StackVal with Cat1 with Ref{
  val innerType: imm.Type
  val backing: Array[_]
  def apply(index: Int): vrt.Val
}
object Arr{
  object Obj{
    class TypeX[T](val t: imm.Type)

    def apply(t: imm.Type.Ref, n: Int) = {
      new Arr.Obj(t, Array.fill[vrt.Val](n)(t.default))
    }
  }
  class Obj(val innerType: imm.Type.Ref, val backing: Array[vrt.Val]) extends Arr{

    val tpe = imm.Type.Arr(innerType)
    def apply(index: Int) = backing(index)
    override def toString = s"vrt.Arr.Obj(${innerType.getClass} ${innerType.unparse}: ${backing.fold("")(_+", "+_)})"
  }
  object Prim{
    def apply[T: ClassTag: imm.Type.Prim.Info](n: Int) = {
      new Arr.Prim(new Array[T](n))
    }
    def unapply(s: vrt.Val) = s match{
      case x: vrt.Arr.Prim[_] => Some(x.backing)
      case _ => None
    }

  }
  class Prim[T: imm.Type.Prim.Info](val backing: Array[T]) extends Arr{
    lazy val innerType: imm.Type.Prim = imm.Type.Prim.Info()
    val tpe = imm.Type.Arr(innerType)
    def charClass = implicitly[imm.Type.Prim.Info[T]]
    def apply(index: Int) = charClass.constructor(backing(index))
    override def toString = s"vrt.PrimArr(${innerType.unparse}: ${backing.fold("")(_+", "+_)})"
  }
}






