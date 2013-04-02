package metascala
package rt



/**
 * A reference to a method with a specific signature
 */
trait Method{
  def sig: imm.Sig
}
object Method{

  /**
   * A reference to a native method at a particular index in the
   * native method table
   */
  case class Native(clsName: String, val sig: imm.Sig, func: natives.Bindings.Func) extends Method{

  }

  /**
   * A reference to a method belonging to a class
   */
  case class Cls(cls: rt.Cls, methodIndex: Int, method: imm.Method)(implicit vm: VM) extends Method{
    lazy val sig = method.sig
    lazy val insns = Array(method.code.insns:_*)
  }
}
