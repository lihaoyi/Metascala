package svm.model

/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 2/26/13
 * Time: 12:44 AM
 * To change this template use File | Settings | File Templates.
 */
package object opcodes {
  object Intish{
    def unapply(x: Any) = x match{
      case b: Boolean => Some(if (b) 1 else 0 )
      case c: Char => Some(c.toInt)
      case b: Byte => Some(b.toInt)
      case s: Short => Some(s.toInt)
      case i: Int => Some(i)
      case _ => None
    }
  }
  case class UnusedOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op = ctx => ???
  }
  implicit def intToByte(n: Int) = n.toByte

  abstract class BaseOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op: Context => Unit
  }

}
