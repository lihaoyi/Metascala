package metascala
package rt

import collection.mutable



import metascala.imm.Sig
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
          val interfaces: Seq[imm.Type.Cls],
          val accessFlags: Int,
          val methods: Seq[rt.Method.Cls],
          val fieldList: Seq[imm.Field],
          val staticList: Seq[imm.Field],
          val outerCls: Option[imm.Type.Cls],
          val index: Int)
         (implicit vm: VMInterface){
  import vm._

  var initialized = false


  def checkInitialized()(implicit vm: VMInterface): Unit = {
    if (!initialized){
      initialized = true
      vm.resolveDirectRef(tpe, Sig("<clinit>", imm.Desc.read("()V")))
        .foreach(threads(0).invoke(_, Agg.empty))

      superType.foreach{ cls =>
        vm.ClsTable(cls).checkInitialized()
      }
    }
  }

  val isInterface = (accessFlags & Access.Interface) != 0
  val statics = vm.alloc(implicit i =>
    rt.Arr.alloc(I, staticList.length)
  )

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
      case Some(superT) => tpe :: ClsTable(superT).clsAncestry
    }
  }

  /**
   * All classes and interfaces that this class inherits from
   */
  lazy val typeAncestry: Set[imm.Type.Cls] = {
    Set(tpe) ++
    superType.toSeq.flatMap(ClsTable(_).typeAncestry) ++
    interfaces.flatMap(ClsTable(_).typeAncestry)
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

    methods.filter(!_.static)
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

  def shortName = Util.shorten(tpe.name)

  def heapSize = fieldList.length + Constants.objectHeaderSize
}


