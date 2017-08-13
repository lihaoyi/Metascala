package metascala
package rt

/**
 * A reference to a method with a specific signature
 */
trait Method{
  def sig: imm.Sig
  lazy val argSize = sig.desc.args.foldLeft(0)(_ + _.size)
}

