package sm
package rt

import collection.mutable


import  sm.{vrt, VM, imm}
import sm.imm.{Access, Type}

class Var(var x: vrt.Val){
  def apply() = x
  def update(y: vrt.Val){
    x = y
  }
}

class Cls(val clsData: imm.Cls, val index: Int)(implicit vm: VM){

  import vm._

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

  def resolveStatic(owner: Type.Cls, name: String) = {
    ancestry.dropWhile(_.tpe != owner)
      .find(_.fields.exists(_.name == name))
      .get.tpe.statics(name)
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


  val methodList: Seq[(rt.Cls, Int)] = {

    val methods =
      mutable.ArrayBuffer(
        clsData.superType
               .toArray
               .flatMap(_.methodList): _*
      )

    clsData.methods
           .filter(_.access.&(Access.Static) == 0)
           .zipWithIndex.map{ case (m, i) =>
      val index = methods.indexWhere{ case (cls, mi) =>
        cls != null &&
        cls.clsData.methods(mi).name == m.name &&
        cls.clsData.methods(mi).desc == m.desc
      }

      val nIndex = vm.natives.trappedIndex.indexWhere{case ((n, idesc), func) =>
        (n == name + "/" + m.name) && (idesc == m.desc)
      }

      (nIndex, index) match{
        case (x, -1) if x != -1 => methods.append((null, x))
        case (x, n) if x != -1 => methods(n) = (null, x)
        case (_, -1) => methods.append((this, i))
        case (_, n) => methods(n) = (this, i)
      }
    }


    methods
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
