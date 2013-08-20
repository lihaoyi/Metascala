package metascala
package rt

import metascala.opcodes.{Code, Conversion}
import org.objectweb.asm.tree.MethodNode


/**
 * A reference to a method with a specific signature
 */
trait Method{
  def sig: imm.Sig
  lazy val argSize = sig.desc.args.foldLeft(0)(_ + _.size)
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
  case class Cls(clsIndex: Int,
                 methodIndex: Int,
                 sig: imm.Sig,
                 accessFlags: Int,
                 codeThunk: () => Code)
                (implicit vm: VM) extends Method{
    lazy val cls = vm.ClsTable.clsIndex(clsIndex)
    lazy val code = codeThunk()

    def static = (accessFlags & imm.Access.Static) != 0
    def native = (accessFlags & imm.Access.Native) != 0
    override def toString = s"Method.Cls(${cls.name}, ${sig.unparse}})"
  }
}
