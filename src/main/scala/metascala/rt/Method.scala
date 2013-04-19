package metascala
package rt



/**
 * A reference to a method with a specific signature
 */
trait Method{
  def sig: imm.Sig
}
object Method{


  case class Native(clsName: String,
                     sig: imm.Sig,
                     func: rt.Thread => Unit)
                     extends Method


  /**
   * A reference to a method belonging to a class
   */
  case class Cls(cls: rt.Cls, methodIndex: Int, method: imm.Method)(implicit vm: VM) extends Method{
    lazy val sig = method.sig
    lazy val insns = Array(method.code.insns:_*)
    override def toString = s"Method.Cls(${cls.name}, ${method.name}${method.sig.unparse}})"
  }
}
