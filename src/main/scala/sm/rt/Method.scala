package sm
package rt



/**
 * A reference to a method with a specific signature
 */
trait Method{
  def name: String
  def desc: imm.Desc
}
object Method{

  /**
   * A reference to a native method at a particular index in the
   * native method table
   */
  case class Native(sig: imm.Method.Sig, func: natives.Bindings.Func)(implicit vm: VM) extends Method{
    lazy val name = sig._1.reverse.takeWhile(_ != '/').reverse
    lazy val desc = sig._2
  }

  /**
   * A reference to a method belonging to a class
   */
  case class Cls(clsIndex: Int, methodIndex: Int, method: imm.Method)(implicit vm: VM) extends Method{
    assert(clsIndex >= 0, "clsIndex can't be negative")
    assert(methodIndex >= 0, "index can't be negative")
    def name = method.name
    def desc = method.desc
    val insns = Array(method.code.insns:_*)
  }
}
