package sm.virt

import collection.mutable
import sm._
import sm.imm.Field
import sm.imm.Access
import sm.imm
import scala.Some
import reflect.ClassTag


trait Val
object Obj{

  def initMembers(cls: imm.Cls, filter: Field => scala.Boolean)(implicit vm: VM): List[Map[String, virt.Val]] = {
    import vm._
    cls.fields.filter(filter).map{f =>
      f.name -> imm.Type.default(f.desc)
    }.toMap :: cls.superType.toList.flatMap(x => initMembers(x.clsData, filter))
  }


  def apply(clsName: String, initMembers: (String, virt.Val)*)(implicit vm: VM) = {
    new Obj(vm.Classes(imm.Type.Cls(clsName)), initMembers: _*)
  }
  def unapply(x: Any) = x match{
    case x: Obj => Some((x.cls.name, x.members))
    case _ => None
  }
}
class Obj(val cls: sm.Cls, initMembers: (String, virt.Val)*)
         (implicit vm: VM) extends Val{ import vm._

  val members =
    Obj.initMembers(cls.clsData, x => (x.access & Access.Static) == 0)
       .map(x => mutable.Map(x.toSeq:_*))
  val magicMembers = mutable.Map[String, Any]()
  for ((s, v) <- initMembers){
    this(imm.Type.Cls.read(cls.name), s) = v
  }

  def apply(owner: imm.Type.Cls, name: String): virt.Val = {
    val start = cls.ancestry.indexWhere(_.tpe == owner)

    members.drop(start)
      .find(_.contains(name))
      .get(name)

  }
  def update(owner: imm.Type.Cls, name: String, value: virt.Val) = {
    val start = cls.ancestry.indexWhere(_.tpe == owner)
    println("Updating " + name )
    members.drop(start)
      .find(_.contains(name))
      .get(name) = value
  }

  def withMagic(x: String, a: Any) = {
    magicMembers(x) = a
    this
  }
  override def toString = {
    s"virt.Obj(${cls.name})"
  }
}

object Arr{
  class TypeX[T](val t: imm.Type)

  def apply(t: imm.Type.Entity, n: scala.Int) = {
    new Arr(t, Array.fill[virt.Val](n)(imm.Type.default(t)))
  }
}
case class Arr(tpe: imm.Type.Entity, backing: Array[virt.Val]) extends Val{
  override def toString = s"virt.Arr(${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
}
case class Boolean(v: scala.Boolean) extends Val
case class Byte(v: scala.Byte) extends Val
case class Char(v: scala.Char) extends Val
case class Short(v: scala.Short) extends Val
case class Int(v: scala.Int) extends Val
case class Float(v: scala.Float) extends Val
case class Long(v: scala.Long) extends Val
case class Double(v: scala.Double) extends Val
case object Null extends Val
case object Unit extends Val


