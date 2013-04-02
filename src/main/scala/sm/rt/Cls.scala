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

/**
 * The runtime mutable and VM-specific data of a Java Class
 */
class Cls(val clsData: imm.Cls, val index: Int)(implicit vm: VM){
  import vm._

  val methods: Seq[rt.Method.Cls] =
    clsData.methods
           .zipWithIndex
           .map{case (m, i) => new rt.Method.Cls(this, i, m)}


  lazy val obj = new vrt.Cls(Type.Cls(name))
  val statics =
    clsData.fields
           .filter(_.static)
           .map{f => f.name -> new Var(f.desc.default) }
           .toMap


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
    clsData.fields.filter(!_.static)
  }


  lazy val vTable: Seq[rt.Method] = {
    val oldMethods =
      mutable.ArrayBuffer(
        clsData.superType
               .toArray
               .flatMap(_.vTable): _*
      )

    methods.filter(!_.method.static)
           .foreach{ m =>

      val index = oldMethods.indexWhere{ mRef => mRef.name == m.name && mRef.desc == m.desc }

      val native = vm.natives.trapped.find{case rt.Method.Native(clsName, imm.Sig(mName, idesc), func) =>
        (name == clsName) && (mName == m.name) && (idesc == m.desc)
      }

      val update =
        if (index == -1) oldMethods.append(_: Method)
        else oldMethods.update(index, _: Method)

      native match {
        case None => update(m)
        case Some(native) => update(native)
      }
    }

    oldMethods
  }

  lazy val vTableMap = vTable.map(m => m.sig -> m).toMap
}


