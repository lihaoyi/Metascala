package metascala
package rt

import metascala.opcodes.Conversion
import org.objectweb.asm.tree.MethodNode


/**
 * A reference to a method with a specific signature
 */
trait Method{
  def sig: imm.Sig
}
object Method{
  case class Native(clsName: String,
                     sig: imm.Sig,
                     func: (rt.Thread, () => Int, Int => Unit) => Unit)
                     extends Method{
    override def toString = s"Method.Native(${clsName}, ${sig.unparse}})"
  }


  /**
   * A reference to a method belonging to a class
   */
  case class Cls(clsIndex: Int, methodIndex: Int, method: imm.Method)(implicit vm: VM) extends Method{
    lazy val cls = vm.ClsTable.clsIndex(clsIndex)
    lazy val sig = method.sig
    lazy val code = Conversion.ssa(cls.name, method.mn)


    override def toString = s"Method.Cls(${cls.name}, ${method.sig.unparse}})"
  }
}
