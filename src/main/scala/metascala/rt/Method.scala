package metascala
package rt

import metascala.natives.Bindings
import metascala.opcodes.Code


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
                     func: (Bindings.Interface, () => Int, Int => Unit) => Unit)
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
                 codeThunk: () => Code)extends Method{
    lazy val code = codeThunk()

    def static = (accessFlags & Access.Static) != 0
    def native = (accessFlags & Access.Native) != 0
  }
}
