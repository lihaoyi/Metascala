package metascala
package vrt



trait Val{
  def tpe: imm.Type
  def toStackVal = this match {
    case vrt.Boolean(b) => vrt.Int(if (b) 1 else 0)
    case vrt.Char(c) => vrt.Int(c)
    case vrt.Byte(b) => vrt.Int(b)
    case vrt.Short(s) => vrt.Int(s)
    case x: vrt.StackVal => x
  }

}

/**
 * Represents a value that can exist on the operand stack of the
 * Metascala VM: everything except booleans, chars, bytes and shorts
 * which are all converted into ints
 */
trait StackVal extends Val{
  def size: scala.Int
}

/**
 * A value of size 1 on the stack (everything but doubles and longs)
 */
trait Cat1 extends StackVal{
  def size: scala.Int = 1
}
/**
 * A value of size 2 on the stack (doubles and longs)
 */
trait Cat2 extends StackVal{
  def size: scala.Int = 2
}