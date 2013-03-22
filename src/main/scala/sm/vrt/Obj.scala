package sm.vrt

import collection.mutable
import sm._
import sm.imm.Field
import sm.imm.Access
import sm.imm

import reflect.ClassTag
import sm.imm.Type.Prim.Info



object Obj{
  import scala.Boolean
  def initMembers(cls: imm.Cls, filter: Field => Boolean)(implicit vm: VM): List[Map[String, vrt.Val]] = {
    import vm._
    cls.fields.filter(filter).map{f =>
      f.name -> f.desc.default
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
         (implicit vm: VM) extends StackVal with Cat1 {
  import vm._

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
  def apply(index: Int): vrt.Val
}
object Arr{
  object Obj{
    class TypeX[T](val t: imm.Type)

    def apply(t: imm.Type.ObjEntity, n: Int) = {
      new Arr.Obj(t, Array.fill[vrt.Val](n)(t.default))
    }
  }
  class Obj(val tpe: imm.Type.ObjEntity, val backing: Array[vrt.Val]) extends Arr{
    assert(tpe != imm.Type.Cls("byte"))
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
    def charClass = implicitly[imm.Type.Prim.Info[T]]
    def apply(index: Int) = charClass.constructor(backing(index))
    override def toString = s"vrt.PrimArr(${tpe.unparse}: ${backing.fold("")(_+", "+_)})"
  }
}






