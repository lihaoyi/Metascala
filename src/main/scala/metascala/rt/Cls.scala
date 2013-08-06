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
class Cls(val tpe: imm.Type.Cls,
          val superType: Option[imm.Type.Cls],
          val sourceFile: Option[String],
          val isInterface: Boolean,
          val interfaces: Seq[imm.Type.Cls],
          val accessFlags: Int,
          val methods: Seq[rt.Method.Cls],
          val fieldList: Seq[imm.Field],
          val staticList: Seq[imm.Field],
          val index: Int)
         (implicit vm: VM){
  import vm._

  var initialized = false
  def this(clsData: imm.Cls, index: Int)(implicit vm: VM){
    this(
      clsData.tpe,
      clsData.superType,
      clsData.misc.sourceFile,
      (clsData.access_flags & 0x0200) != 0,
      clsData.interfaces,
      clsData.access_flags,
      clsData.methods
        .zipWithIndex
        .map{case (m, i) => new rt.Method.Cls(index, i, m)},
      clsData.superType.toSeq.flatMap(_.cls.fieldList) ++
        clsData.fields.filter(!_.static).flatMap{x =>
          Seq.fill(x.desc.size)(x)
        },
      clsData.fields.filter(_.static).flatMap{x =>
        Seq.fill(x.desc.size)(x)
      },
      index
    )
  }
  def checkInitialized()(implicit vm: VM): Unit = {
    if (!initialized){
      initialized = true
      vm.resolveDirectRef(tpe, Sig("<clinit>", imm.Desc.read("()V")))
        .foreach(threads(0).invoke(_, Nil))



      superType.foreach{ cls =>
        vm.ClsTable(cls).checkInitialized()
      }
    }
  }


  val statics = new Array[Int](staticList.length)

  def method(name: String, desc: imm.Desc): Option[rt.Method] = {
    clsAncestry.flatMap(_.methods)
               .find(m => m.sig.name == name && m.sig.desc == desc)
  }

  lazy val size = fieldList.length

  def name = tpe.name

  /**
   * All classes that this class inherits from
   */
  lazy val clsAncestry: List[imm.Type.Cls] = {
    superType match{
      case None => List(tpe)
      case Some(superT) => tpe :: superT.cls.clsAncestry
    }
  }

  /**
   * All classes and interfaces that this class inherits from
   */
  lazy val typeAncestry: Set[imm.Type.Cls] = {
    Set(tpe) ++
    superType.toSeq.flatMap(_.cls.typeAncestry) ++
    interfaces.flatMap(_.cls.typeAncestry)
  }


  /**
   * The virtual function dispatch table
   */
  lazy val vTable: Seq[rt.Method] = {
    val oldMethods =
      mutable.ArrayBuffer(
        superType
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
    s"Cls($index, ${tpe.name})"
  }

  def shortName = shorten(tpe.name)

  def heapSize = fieldList.length + rt.Obj.headerSize
}


