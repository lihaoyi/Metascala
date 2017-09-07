package metascala
package rt

import metascala.util._

import scala.collection.mutable

trait ClsTable0 extends rt.Arr.ClsTable with (imm.Type.Cls => rt.Cls){
  def indexOfType(tpe: imm.Type.Cls): Int = this(tpe).index
  def typeAtIndex(i: Int): imm.Type = this.clsIndex(i).tpe
  def calcFromBytes(x: imm.Type.Cls, input: Array[Byte], cpPatches: Map[Int, Int] = null): rt.Cls
  val clsIndex: mutable.ArrayBuffer[rt.Cls]
}

object Cls{

  trait VMInterface extends rt.Arr.VMInterface{
    implicit def clsTable: ClsTable0
    def lookupNatives(expectedCls: imm.Type.Cls, sig: imm.Sig): Option[Int => rt.Method]
  }

  /**
    * A list of field metadata of varying width; used to efficiently look up
    * fields by slots, slots by fields, or enumerate fields with their slots.
    *
    * Logic is shared between both instance fields and static fields.
    */
  class FieldSlotList(label: String, javaName: String, list: Seq[imm.Field]){
    private[this] val lookupArray: IndexedSeq[imm.Field] =
      list.iterator.flatMap(x => Seq(x).padTo(x.desc.size, null)).toArray[imm.Field]

    def slottedList = lookupArray.zipWithIndex.collect{case (f, i) if f != null => (f, i)}
    def rawList = lookupArray.filter(_ != null)
    def get(index: Int) = {
      lookupArray(index) match {
        case null => throw new Exception("Invalid field offset: " + index)
        case v => v
      }
    }
    def get(name: String) = {
      getIndex0(name) match {
        case -1 => throw new Exception(s"Invalid field name [$name] in class [$javaName]")
        case v => lookupArray(v)
      }
    }

    def getIndex0(name: String) = {
      lookupArray.lastIndexWhere(x => x != null && x.name == name)
    }
    def getIndex(name: String) = {
      val base = getIndex0(name)
      if (base == -1) throw new Exception(s"Cannot find $label [$name] in class [$javaName]")
      else base
    }
    def slotCount = lookupArray.length
  }
}

/**
 * The runtime mutable and VM-specific data of a Java Class
 */
class Cls(val tpe: imm.Type.Cls,
          val superType: Option[imm.Type.Cls],
          val sourceFile: Option[String],
          val interfaces: Seq[imm.Type.Cls],
          val innerClasses: Seq[imm.Type.Cls],
          val accessFlags: Int,
          val methods: Seq[rt.ClsMethod],
          val allFieldList: Seq[imm.Field],
          val outerCls: Option[imm.Type.Cls],
          val index: Int)
         (implicit vm: Cls.VMInterface){

  val (fieldList0, staticList0) = allFieldList.partition(!_.static)
  import vm._

  var initialized = false

  val isInterface = (accessFlags & Access.Interface) != 0
  var statics: WritableRef = null

  def method(name: String, desc: imm.Desc): Option[rt.Method] = {
    clsAncestry.flatMap(_.methods)
               .find(m => m.sig.name == name && m.sig.desc == desc)
  }


  val fieldInfo: Cls.FieldSlotList = new Cls.FieldSlotList(
    "field", tpe.javaName,
    superType.toSeq.flatMap(_.fieldInfo.rawList) ++ fieldList0
  )
  val staticInfo = new Cls.FieldSlotList("static", tpe.javaName, staticList0)

  lazy val size = fieldInfo.slotCount

  /**
   * All classes that this class inherits from
   */
  lazy val clsAncestry: List[imm.Type.Cls] = {
    superType match{
      case None => List(tpe)
      case Some(superT) => tpe :: clsTable(superT).clsAncestry
    }
  }

  /**
   * All classes and interfaces that this class inherits from
   */
  lazy val typeAncestry: Set[imm.Type.Cls] = {
    Set(tpe) ++
    superType.toSeq.flatMap(clsTable(_).typeAncestry) ++
    interfaces.flatMap(clsTable(_).typeAncestry)
  }


  def computeDispatchTable(getParent: imm.Type.Cls => IndexedSeq[rt.Method],
                           static: Boolean) = {
    val oldMethods =
      mutable.ArrayBuffer(
        superType
          .toArray
          .flatMap(getParent): _*
      )

    for(m <- methods) if (m.static == static){
      val actualMethod = vm.lookupNatives(tpe, m.sig) match {
        case None => m
        case Some(native) => native(m.accessFlags)
      }
      val index = oldMethods.indexWhere{ mRef => mRef.sig == m.sig }
      if (index == -1) oldMethods.append(actualMethod)
      // Both instance *and* static methods can be overriden by subclasses.
      // Bet you didn't know that!
      else oldMethods.update(index, actualMethod)
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
  lazy val interfaceMethodMap: Map[imm.Sig, rt.Method] = {
    val init = mutable.Map(vTable.map(m => m.sig -> m):_*)
    for (interface <- superType ++ interfaces){
      for((sig, impl) <- interface.interfaceMethodMap){
        if (!init.contains(sig)){
          init(sig) = impl
        }
      }
    }
    init.toMap
  }
  def lookupInterfaceMethod(sig: imm.Sig) = {
    interfaceMethodMap.getOrElse(
      sig,
      throw new Exception(
        "Unable to find signature [" + sig.toString + "] in class " + tpe.javaName + "\n" +
        "available signatures:\n    " + interfaceMethodMap.keysIterator.mkString("\n    ")
      )
    )
  }

  override def toString() = {
    s"Cls($index, ${tpe.javaName})"
  }

  def shortName = Util.shorten(tpe.javaName)

  def heapSize = fieldInfo.slotCount + Constants.objectHeaderSize
}


