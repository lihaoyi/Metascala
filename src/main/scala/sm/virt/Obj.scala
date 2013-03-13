package sm.virt

import collection.mutable
import sm.{Cls, imm, virt, VM}
import imm.Field
import imm.Type
import imm.Access
import reflect.ClassTag

object Obj{

  def initMembers(cls: imm.Cls, filter: Field => Boolean)(implicit vm: VM): List[Map[String, Any]] = {
    import vm._
    cls.fields.filter(filter).map{f =>
      f.name -> imm.Type.default(f.desc)
    }.toMap :: cls.superType.toList.flatMap(x => initMembers(x.clsData, filter))
  }


  def apply(clsName: String, initMembers: (String, Any)*)(implicit vm: VM) = {
    new Obj(vm.Classes(imm.Type.Cls(clsName)), initMembers: _*)
  }
  def unapply(x: Any) = x match{
    case x: Obj => Some((x.cls.name, x.members))
    case _ => None
  }
}
class Obj(val cls: sm.Cls, initMembers: (String, Any)*)
         (implicit vm: VM){ import vm._

  val members = Obj.initMembers(cls.clsData, x => (x.access & Access.Static) == 0).map(x => mutable.Map(x.toSeq:_*))
  val magicMembers = mutable.Map[String, Any]()
  for ((s, v) <- initMembers){
    this(imm.Type.Cls.read(cls.name), s) = v
  }

  def apply(owner: imm.Type.Cls, name: String) = {
    val start = cls.ancestry.indexWhere(_.tpe == owner)

    members.drop(start)
      .find(_.contains(name))
      .get(name)

  }
  def update(owner: imm.Type.Cls, name: String, value: Any) = {
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

  def apply(t: imm.Type, n: Int) = {
    new Arr(t, Array.fill[Any](n)(imm.Type.default(t)))
  }
}
case class Arr(val tpe: imm.Type, val backing: Array[Any]){
  override def toString = s"virt.Arr(${tpe.name}: ${backing.fold("")(_+", "+_)})"
}

