package sm.virt

import collection.mutable
import sm._
import sm.imm.Field
import sm.imm.Access
import sm.imm
import scala.Some
import reflect.ClassTag

object Val{
  implicit def virtBoolean(i: scala.Boolean) = Boolean(i)
  implicit def virtByte(i: scala.Byte) = Byte(i)
  implicit def virtChar(i: scala.Char) = Char(i)
  implicit def virtShort(i: scala.Short) = Short(i)
  implicit def virtInt(i: scala.Int) = Int(i)
  implicit def virtFloat(i: scala.Float) = Float(i)
  implicit def virtLong(i: scala.Long) = Long(i)
  implicit def virtDouble(i: scala.Double) = Double(i)
  implicit def virtNull(i: scala.Null) = Null
  implicit def virtUnit(i: scala.Unit) = Unit
  implicit def virtArray[T <% Val](i: Array[T]) = new Arr(imm.Type.Cls("java/lang/Object"), i.map(x => x: Val))

  implicit def unvirtBoolean(i: Boolean) = i.v
  implicit def unvirtByte(i: Byte) = i.v
  implicit def unvirtChar(i: Char) = i.v
  implicit def unvirtShort(i: Short) = i.v
  implicit def unvirtInt(i: Int) = i.v
  implicit def unvirtFloat(i: Float) = i.v
  implicit def unvirtLong(i: Long) = i.v
  implicit def unvirtDouble(i: Double) = i.v
  implicit def unvirtNull(i: Null) = null
  implicit def unvirtUnit(i: Unit) = ()
  implicit def unvirtArray[T](i: Arr)(implicit ct: ClassTag[T], f: Val => T) = {
    i.backing.map(x => x: T)
  }

  implicit def virtString(i: String)(implicit vm: VM) = {
    virt.Obj("java/lang/String",
      "value" -> virt.Arr(imm.Type.Arr(imm.Type.Prim("C")), i.toCharArray.map(x => virtChar(x) : Val))
    )
  }
  implicit def unvirtString(i: virt.Obj) = {
    new String(i.members(0)("value").cast[virt.Arr].backing.map{case x: virt.Char => x.v})
  }
}
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

  def apply(t: imm.Type, n: scala.Int) = {
    new Arr(t, Array.fill[virt.Val](n)(imm.Type.default(t)))
  }
}
case class Arr(tpe: imm.Type, backing: Array[virt.Val]) extends Val{
  override def toString = s"virt.Arr(${tpe.name}: ${backing.fold("")(_+", "+_)})"
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


