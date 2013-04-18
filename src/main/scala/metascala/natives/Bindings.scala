package metascala
package natives

object Bindings{
  val default = new Default {}
  type Func1 = rt.Thread => Seq[Val] => Val
  type Func2 = rt.Thread => Seq[Val] => Long
}

/**
 * A set of bindings between method signatures and callable binding functions
 */
trait Bindings{
  val trapped: Seq[rt.Method.Native1]
  val fileLoader: String => Option[Array[Byte]]
}

