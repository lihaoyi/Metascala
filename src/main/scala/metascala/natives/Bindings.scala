package metascala
package natives

object Bindings{
  val default = new Default {

  }
}

/**
 * A set of bindings between method signatures and callable binding functions
 */
trait Bindings{
  val trapped: Seq[rt.Method.Native]
  val fileLoader: String => Option[Array[Byte]]
}

