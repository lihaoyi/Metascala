package sm
package natives

object Bindings{
  val default = new Default {}
  type Func = rt.Thread => Seq[vrt.Val] => vrt.Val
}

/**
 * A set of bindings between method signatures and callable binding functions
 */
trait Bindings{
  val trapped: Map[imm.Method.Sig, Bindings.Func]
  val trappedIndex: Seq[(imm.Method.Sig, Bindings.Func)]
  val fileLoader: String => Option[Array[Byte]]
}

