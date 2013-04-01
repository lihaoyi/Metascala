package sm
package rt

import collection.mutable


import  sm.{vrt, VM, imm}
import sm.imm.{Access, Type}

/**
 * A handle to a readable and writable value.
 */
final class Var(var x: vrt.Val){
  final def apply() = x
  final def update(y: vrt.Val){
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


  def method(name: String, desc: imm.Desc): Option[imm.Method] = {
    clsAncestry.flatMap(_.methods)
               .find(m => m.name == name && m.desc == desc)
  }

  def resolveStatic(owner: Type.Cls, name: String) = {
    clsAncestry.dropWhile(_.tpe != owner)
      .find(_.fields.exists(_.name == name))
      .get.tpe.statics(name)
  }

  def apply(owner: Type.Cls, name: String) = {
    resolveStatic(owner, name)()
  }

  def update(owner: Type.Cls, name: String, value: vrt.Val) = {
    resolveStatic(owner, name)() = value
  }

  def name = clsData.tpe.name

  lazy val clsAncestry: List[imm.Cls] = {
    clsData.superType match{
      case None => List(clsData)
      case Some(tpe) => clsData :: tpe.cls.clsAncestry
    }
  }

  lazy val typeAncestry: Set[imm.Type.Cls] = {
    Set(this.clsData.tpe) ++
    clsData.superType.toSeq.flatMap(_.cls.typeAncestry) ++
    clsData.interfaces.flatMap(_.cls.typeAncestry)
  }

  val fieldList: Seq[imm.Field] = {
    clsData.superType.toSeq.flatMap(_.fieldList) ++
    clsData.fields.filter(_.access.&(Access.Static) == 0)
  }


  lazy val methodList: Seq[Method] = {
    val methods =
      mutable.ArrayBuffer(
        clsData.superType
               .toArray
               .flatMap(_.methodList): _*
      )

    clsData.methods
           .zipWithIndex
           .filter(_._1.access.&(Access.Static) == 0)
           .map{ case (m, i) =>

      val index = methods.indexWhere{ mRef => mRef.name == m.name && mRef.desc == m.desc }

      val nIndex = vm.natives.trappedIndex.indexWhere{case ((n, idesc), func) =>
        (n == name + "/" + m.name) && (idesc == m.desc)
      }

      val update =
        if (index == -1) methods.append(_: Method)
        else methods.update(index, _: Method)

      nIndex match {
        case -1 => update(Method.Cls(this.index, i))
        case n => update(Method.Native(n))

      }
    }

    methods
  }

  val methodMap: mutable.Map[(String, imm.Desc), Method] = mutable.Map.empty
}


