package svm

import collection.mutable

import svm.{VM, imm}
import imm.Type
class Cls(val classData: imm.Cls,
          val statics: mutable.Map[String, Any] = mutable.Map.empty)
         (implicit vm: VM){

  import vm._

  classData.superType.map(vm.Classes)
  lazy val obj = new virt.Cls(Type.Cls(name))

  classData.fields.map{f =>
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

  def name = classData.tpe.name

  def ancestry = {
    def rec(cd: imm.Cls): List[imm.Cls] = {
      cd.superType match{
        case None => List(cd)
        case Some(x) => cd :: rec(x.classData)
      }
    }
    rec(classData)
  }

  def checkIsInstanceOf(desc: Type)(implicit vm: VM): Boolean = {
    import vm._

    val res =
      classData.tpe == desc ||
      classData.interfaces.contains(desc) ||
      classData.superType
          .map(l => l.checkIsInstanceOf(desc))
          .getOrElse(false)

    res
  }
}
