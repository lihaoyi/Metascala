package sm.rt

import collection.mutable


import  sm.{vrt, VM, imm}
import sm.imm.{Access, Type}

class Var(var x: vrt.Val){
  def apply() = x
  def update(y: vrt.Val){
    x = y
  }
}

class Cls(val clsData: imm.Cls)(implicit vm: VM){

  import vm._

  clsData.superType.map(vm.Classes)

  val insns = clsData.methods.map(x => mutable.Seq(x.code.insns:_*))
  lazy val obj = new vrt.Cls(Type.Cls(name))
  val statics =
    clsData.fields.map{f =>
      f.name -> new Var(f.desc.default)
    }.toMap


  def method(name: String, desc: Type.Desc): Option[imm.Method] = {
    ancestry.flatMap(_.methods)
            .find(m => m.name == name && m.desc == desc)
  }

  val staticCache = mutable.Map.empty[(Type.Cls, String), Var]
  def resolveStatic(owner: Type.Cls, name: String) = {
    staticCache.getOrElseUpdate((owner, name),
      ancestry.dropWhile(_.tpe != owner)
        .find(_.fields.exists(_.name == name))
        .get.tpe.statics(name)
    )

  }
  def apply(owner: Type.Cls, name: String) = {
    resolveStatic(owner: Type.Cls, name: String)()
  }

  def update(owner: Type.Cls, name: String, value: vrt.Val) = {
    resolveStatic(owner: Type.Cls, name: String)() = value
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

  val fieldList: Seq[imm.Field] = {
    clsData.superType.toSeq.flatMap(_.fieldList) ++ clsData.fields.filter(_.access.&(Access.Static) == 0)
  }
  def resolveField(owner: imm.Type.Cls, name: String) = {
    fieldList.lastIndexWhere(_.name == name)
  }

  def checkIsInstanceOf(desc: Type)(implicit vm: VM): Boolean = {
    import vm._

    val res =
      clsData.tpe == desc ||
      clsData.interfaces.exists(_.checkIsInstanceOf(desc)) ||
      clsData.superType
             .map(l => l.checkIsInstanceOf(desc))
             .getOrElse(false)
    res
  }
}
