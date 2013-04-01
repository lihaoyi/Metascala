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
  case class Native(clsName: String, sig: imm.Method.Sig, func: natives.Bindings.Func) extends Method{
    def name = sig._1
    def desc = sig._2
  }

  /**
   * A reference to a method belonging to a class
   */
  case class Cls(clsIndex: Int, methodIndex: Int, method: imm.Method)(implicit vm: VM) extends Method{
    def name = method.name
    def desc = method.desc
    val insns = Array(method.code.insns:_*)
  }
}
