package metascala
package rt

/**
 * A reference to a method with a specific signature
 */
trait Method{
  def sig: imm.Sig
  def static: Boolean

  def localsSize: Int
}

