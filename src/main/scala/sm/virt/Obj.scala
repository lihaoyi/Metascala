package sm.virt

import collection.mutable
import sm._
import sm.imm.Field
import sm.imm.Access
import sm.imm
import scala.Some
import reflect.ClassTag


trait Val
object Val{
  implicit class stackable(s: virt.Val){
    def toStackVal = s match {
      case Boolean(b) => virt.Int(if (b) 1 else 0)
      case Char(c) => virt.Int(c)
      case Byte(b) => virt.Int(b)
      case Short(s) => virt.Int(s)
      case x: virt.StackVal => x
    }
  }
}

trait StackVal extends Val

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
         (implicit vm: VM) extends StackVal{ import vm._

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

trait Arr extends StackVal{
  val tpe: imm.Type.Entity
  val backing: Array[_]
}
object ObjArr{
  class TypeX[T](val t: imm.Type)

  def apply(t: imm.Type.Entity, n: scala.Int) = {
    new ObjArr(t, Array.fill[virt.Val](n)(imm.Type.default(t)))
  }
}
class ObjArr(val tpe: imm.Type.Entity, val backing: Array[virt.Val]) extends Arr{
  override def toString = s"virt.ObjArr(${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
}
object PrimArr{
  def apply[T <: AnyVal : ClassTag](t: Char, n: scala.Int) = {
    new PrimArr(imm.Type.Prim(t), new Array[T](n))
  }
  def unapply(s: virt.Val) = s match{
    case x: virt.PrimArr[_] => Some((x.tpe.char, x.backing))
    case _ => None
  }

}
class PrimArr[T <: AnyVal](val tpe: imm.Type.Prim, val backing: Array[T]) extends Arr{
  override def toString = s"virt.PrimArr(${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
}
trait WrapVal[T]{
  def v: T
}
case class Boolean(v: scala.Boolean) extends WrapVal[scala.Boolean] with Val
case class Byte(v: scala.Byte) extends WrapVal[scala.Byte] with Val
case class Char(v: scala.Char) extends WrapVal[scala.Char] with Val
case class Short(v: scala.Short) extends WrapVal[scala.Short] with Val
case class Int(v: scala.Int) extends WrapVal[scala.Int] with StackVal
case class Float(v: scala.Float) extends WrapVal[scala.Float] with StackVal
case class Long(v: scala.Long) extends WrapVal[scala.Long] with StackVal
case class Double(v: scala.Double) extends WrapVal[scala.Double] with StackVal
case object Null extends WrapVal[scala.Null] with StackVal{
  def v = null
}
case object Unit extends WrapVal[scala.Unit] with StackVal{
  def v = ()
}


