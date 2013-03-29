package sm
package vrt

import collection.mutable
import sm._
import rt.Var
import sm.imm.Field
import sm.imm.Access

import reflect.ClassTag
import scala.Some

import sm.imm


trait Ref{
  def refType: imm.Type.Ref
}
object Obj{
  import scala.Boolean



  def apply(clsName: String, initMembers: (String, vrt.Val)*)(implicit vm: VM) = {
    new Obj(vm.Classes(imm.Type.Cls(clsName)), initMembers: _*)
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

  var magicMembers = Map[String, Any]()

  for ((s, v) <- initMembers){
    this(imm.Type.Cls.read(cls.name), s) = v
  }

  def refType = cls.clsData.tpe


  def apply(owner: imm.Type.Cls, name: String): vrt.Val = {

    val x = members(owner.resolveField(owner, name))._2()
    x
  }

  def update(owner: imm.Type.Cls, name: String, value: vrt.Val) = {

    val x = members(owner.resolveField(owner, name))._2() = value
    x
  }

  def withMagic(x: String, a: Any) = {
    magicMembers = magicMembers.updated(x, a)
    this
  }
  override def toString = {
    s"vrt.Obj(${cls.name})"
  }
}

trait Arr extends StackVal with Cat1 with Ref{
  val tpe: imm.Type.Entity
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
  class Obj(val tpe: imm.Type.Ref, val backing: Array[vrt.Val]) extends Arr{

    val refType = imm.Type.Arr(tpe)
    def apply(index: Int) = backing(index)
    override def toString = s"vrt.Arr.Obj(${tpe.getClass} ${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
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
    lazy val tpe: imm.Type.Prim = imm.Type.Prim.Info()
    val refType = imm.Type.Arr(tpe)
    def charClass = implicitly[imm.Type.Prim.Info[T]]
    def apply(index: Int) = charClass.constructor(backing(index))
    override def toString = s"vrt.PrimArr(${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
  }
}






