package metascala
package rt

import metascala.util.{Access, Constants, Ref, Util}

import scala.collection.mutable

trait ClsTable extends rt.Arr.ClsTable with (imm.Type.Cls => rt.Cls){
  def indexOfType(tpe: imm.Type.Cls): Int = this(tpe).index
  def typeAtIndex(i: Int): imm.Type = this.clsIndex(i).tpe
  val clsIndex: mutable.ArrayBuffer[rt.Cls]
}

object Cls{

  trait VMInterface extends rt.Arr.VMInterface{
    implicit def ClsTable: ClsTable
    def lookupNatives(name: String, sig: imm.Sig): Option[rt.Method]
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
          val methods: Seq[rt.ClsMethod],
          val fieldList: Seq[imm.Field],
          val staticList: Seq[imm.Field],
          val outerCls: Option[imm.Type.Cls],
          val index: Int)
         (implicit vm: Cls.VMInterface){
  import vm._

  var initialized = false




  val isInterface = (accessFlags & Access.Interface) != 0
  var statics: Ref = null

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


  def computeDispatchTable(getParent: imm.Type.Cls => IndexedSeq[rt.Method],
                           static: Boolean) = {
    val overrides = !static
    val oldMethods =
      mutable.ArrayBuffer(
        superType
          .toArray
          .flatMap(getParent): _*
      )

    for(m <- methods) if (m.static == static){
      val actualMethod = vm.lookupNatives(name, m.sig) match {
        case None => m
        case Some(native) => native
      }
      if (!overrides) oldMethods.append(actualMethod)
      else{

        val index = oldMethods.indexWhere{ mRef => mRef.sig == m.sig }
          if (index == -1) oldMethods.append(actualMethod)
          else oldMethods.update(index, actualMethod)
      }


    }
    oldMethods
  }
  /**
   * The virtual function dispatch table
   */
  lazy val vTable: IndexedSeq[rt.Method] = {
    computeDispatchTable(_.vTable, static = false)
  }

  /**
   * The static function dispatch table
   */
  lazy val staticTable: IndexedSeq[rt.Method] = {
    computeDispatchTable(_.staticTable, static = true)
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


