package sm.vrt

import collection.mutable
import sm._
import sm.imm.Field
import sm.imm.Access
import sm.imm

import reflect.ClassTag
import sm.imm.Type.CharClass



object Obj{
  import scala.Boolean
  def initMembers(cls: imm.Cls, filter: Field => Boolean)(implicit vm: VM): List[Map[String, vrt.Val]] = {
    import vm._
    cls.fields.filter(filter).map{f =>
      f.name -> imm.Type.CharClass.default(f.desc)
    }.toMap :: cls.superType.toList.flatMap(x => initMembers(x.clsData, filter))
  }


  def apply(clsName: String, initMembers: (String, vrt.Val)*)(implicit vm: VM) = {
    new Obj(vm.Classes(imm.Type.Cls(clsName)), initMembers: _*)
  }
  def unapply(x: Any) = x match{
    case x: Obj => Some((x.cls.name, x.members))
    case _ => None
  }
}
class Obj(val cls: sm.Cls, initMembers: (String, vrt.Val)*)
         (implicit vm: VM) extends StackVal with Cat1{ import vm._

  val members =
    Obj.initMembers(cls.clsData, x => (x.access & Access.Static) == 0)
       .map(x => mutable.Map(x.toSeq:_*))
  val magicMembers = mutable.Map[String, Any]()
  for ((s, v) <- initMembers){
    this(imm.Type.Cls.read(cls.name), s) = v
  }

  def apply(owner: imm.Type.Cls, name: String): vrt.Val = {
    val start = cls.ancestry.indexWhere(_.tpe == owner)

    members.drop(start)
           .find(_.contains(name))
           .get(name)

  }
  def update(owner: imm.Type.Cls, name: String, value: vrt.Val) = {
    val start = cls.ancestry.indexWhere(_.tpe == owner)

    members.drop(start)
           .find(_.contains(name))
           .get(name) = value
  }

  def withMagic(x: String, a: Any) = {
    magicMembers(x) = a
    this
  }
  override def toString = {
    s"vrt.Obj(${cls.name})"
  }
}

trait Arr extends StackVal with Cat1{
  val tpe: imm.Type.Entity
  val backing: Array[_]
}
object ObjArr{
  class TypeX[T](val t: imm.Type)

  def apply(t: imm.Type.Entity, n: Int) = {
    new ObjArr(t, Array.fill[vrt.Val](n)(imm.Type.CharClass.default(t)))
  }
}
class ObjArr(val tpe: imm.Type.Entity, val backing: Array[vrt.Val]) extends Arr{
  override def toString = s"vrt.ObjArr(${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
}
object PrimArr{
  def apply[T: ClassTag: CharClass](n: Int) = {
    new PrimArr(new Array[T](n))
  }
  def unapply(s: vrt.Val) = s match{
    case x: vrt.PrimArr[_] => Some(x.backing)
    case _ => None
  }

}
class PrimArr[T: CharClass](val backing: Array[T]) extends Arr{
  lazy val tpe: imm.Type.Prim = CharClass()
  override def toString = s"vrt.PrimArr(${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
}





