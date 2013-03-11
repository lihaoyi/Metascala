package sm

import collection.mutable

import sm.{VM, imm}
import imm.Type
class Cls(val clsData: imm.Cls,
          val statics: mutable.Map[String, Any] = mutable.Map.empty)
         (implicit vm: VM){

  import vm._

  clsData.superType.map(vm.Classes)
  lazy val obj = new virt.Cls(Type.Cls(name))

  clsData.fields.map{f =>
    statics(f.name) = virt.Obj.initField(f.desc)
  }

  def method(name: String, desc: Type.Desc): Option[imm.Method] = {
    ancestry.flatMap(_.methods).find(m => m.name == name && m.desc == desc)
  }

  def apply(owner: Type.Cls, name: String) = {
    this.ancestry.dropWhile(_.tpe != owner)
                 .find(_.fields.exists(_.name == name))
                 .get.tpe.statics(name)
  }

  def update(owner: Type.Cls, name: String, value: Any) = {
    this.ancestry.dropWhile(_.tpe != owner)
      .find(_.fields.exists(_.name == name))
      .get.tpe.statics(name) = value
  }

  def name = clsData.tpe.name

  val ancestry = {
    def rec(cd: imm.Cls): List[imm.Cls] = {
      cd.superType match{
        case None => List(cd)
        case Some(x) => cd :: rec(x.clsData)
      }
    }
    rec(clsData)
  }

  def checkIsInstanceOf(desc: Type)(implicit vm: VM): Boolean = {
    import vm._

    val res =
      clsData.tpe == desc ||
      clsData.interfaces.contains(desc) ||
      clsData.superType
          .map(l => l.checkIsInstanceOf(desc))
          .getOrElse(false)

    res
  }
}
