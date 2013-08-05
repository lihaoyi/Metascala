package metascala
package rt

import collection.mutable


import  metascala.{VM, imm}
import metascala.imm.{Sig, Access, Type}
import metascala.opcodes.Insn
import metascala.imm.Type.Prim.I

/**
 * A handle to a readable and writable value.
 */
class Var(var x: Val){
  final def apply() = x
  final def update(y: Val){
    x = y
  }
}

/**
 * The runtime mutable and VM-specific data of a Java Class
 */
class Cls(val clsData: imm.Cls, val index: Int)(implicit vm: VM){
  import vm._

  var initialized = false
  def checkInitialized()(implicit vm: VM): Unit = {
    if (!initialized){
      initialized = true
      vm.resolveDirectRef(clsData.tpe, Sig("<clinit>", imm.Desc.read("()V")))
        .foreach(threads(0).invoke(_, Nil))



      clsData.superType.foreach{ cls =>
        vm.ClsTable(cls).checkInitialized()
      }
    }
  }

  /**
   * Mutable representations of all the methods this class has
   */
  val methods: Seq[rt.Method.Cls] =
    clsData.methods
           .zipWithIndex
           .map{case (m, i) => new rt.Method.Cls(this, i, m)}


  val staticList: Seq[imm.Field] = {
    clsData.fields.filter(_.static).flatMap{x =>
      Seq.fill(x.desc.size)(x)
    }
  }

  val statics = new Array[Int](staticList.length)

  def method(name: String, desc: imm.Desc): Option[imm.Method] = {
    clsAncestry.flatMap(_.methods)
               .find(m => m.name == name && m.desc == desc)
  }

  lazy val size = fieldList.length

  def name = clsData.tpe.name

  /**
   * All classes that this class inherits from
   */
  lazy val clsAncestry: List[imm.Cls] = {
    clsData.superType match{
      case None => List(clsData)
      case Some(tpe) => clsData :: tpe.cls.clsAncestry
    }
  }

  /**
   * All classes and interfaces that this class inherits from
   */
  lazy val typeAncestry: Set[imm.Type.Cls] = {
    Set(this.clsData.tpe) ++
    clsData.superType.toSeq.flatMap(_.cls.typeAncestry) ++
    clsData.interfaces.flatMap(_.cls.typeAncestry)
  }



  val fieldList: Seq[imm.Field] = {
    clsData.superType.toSeq.flatMap(_.fieldList) ++
    clsData.fields.filter(!_.static).flatMap{x =>
      Seq.fill(x.desc.size)(x)
    }
  }

  /**
   * The virtual function dispatch table
   */
  lazy val vTable: Seq[rt.Method] = {
    val oldMethods =
      mutable.ArrayBuffer(
        clsData.superType
               .toArray
               .flatMap(_.vTable): _*
      )

    methods.filter(!_.method.static)
           .foreach{ m =>

      val index = oldMethods.indexWhere{ mRef => mRef.sig == m.sig }

      val native = vm.natives.trapped.find{case rt.Method.Native(clsName, sig, func) =>
        (name == clsName) && sig == m.sig
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

  /**
   * A hash map of the virtual function table, used for quick lookup
   * by method signature
   */
  lazy val vTableMap = vTable.map(m => m.sig -> m).toMap

  override def toString() = {
    s"Cls($index, ${clsData.tpe.name})"
  }

  def shortName = shorten(clsData.tpe.name)
}


