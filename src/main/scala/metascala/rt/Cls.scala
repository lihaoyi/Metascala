package metascala
package rt

import collection.mutable


import  metascala.{VM, imm}
import metascala.imm.{Sig, Access, Type}
import metascala.opcodes.{Conversion, Insn}
import metascala.imm.Type.Prim.I
import org.objectweb.asm.tree.ClassNode

/**
 * A handle to a readable and writable value.
 */
class Var(var x: Val){
  final def apply() = x
  final def update(y: Val){
    x = y
  }
}

object Cls{
  def apply(cn: ClassNode, index: Int)(implicit vm: VM) = {

    import imm.NullSafe._
    val fields = cn.fields.safeSeq.map(imm.Field.read)
    val superType = cn.superName.safeOpt.map(Type.Cls.read)
    new Cls(
      tpe = imm.Type.Cls.read(cn.name),
      superType = superType,
      sourceFile = cn.sourceFile.safeOpt,
      interfaces = cn.interfaces.safeSeq.map(Type.Cls.read),
      accessFlags = cn.access,
      methods =
        cn.methods
          .safeSeq
          .zipWithIndex
          .map{case (mn, i) =>
          new rt.Method.Cls(
            index,
            i,
            Sig(mn.name, imm.Desc.read(mn.desc)),
            mn.access,
            () => Conversion.ssa(cn.name, mn)
          )
        },
      fieldList =
        superType.toSeq.flatMap(_.cls.fieldList) ++
          fields.filter(!_.static).flatMap{x =>
            Seq.fill(x.desc.size)(x)
          },
      staticList =
        fields.filter(_.static).flatMap{x =>
          Seq.fill(x.desc.size)(x)
        },
      outerCls = cn.outerClass.safeOpt.map(Type.Cls.read),
      index
    )
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
         (implicit vm: VM){
  import vm._

  var initialized = false


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

  val isInterface = (accessFlags & Access.Interface) != 0
  val statics = vm.alloc(implicit i =>
    rt.Arr.allocate(I, staticList.length)
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

  def shortName = shorten(tpe.name)

  def heapSize = fieldList.length + rt.Obj.headerSize
}


