package svm.imm

import svm.VmThread
import svm.virt
package object opcodes {
  def ext(x: Any) = {
    x match {
      case b: Boolean => if (b) 1 else 0
      case c: Char => c.toInt
      case b: Byte => b.toInt
      case s: Short => s.toInt
      case x => x
    }
  }
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
  object Cat1{
    def unapply(x: Any): Option[Any] = x match{
      case b: Boolean => Some(b)
      case c: Char => Some(c)
      case b: Byte => Some(b)
      case s: Short => Some(s)
      case i: Int => Some(i)
      case f: Float => Some(f)
      case a: Array[_] => Some(a)
      case o: virt.Obj => Some(o)
      case _ => None
    }
  }
  object Cat2{
    def unapply(x: Any): Option[Any] = x match{
      case l: Long => Some(l)
      case d: Double => Some(d)
      case _ => None

    }
  }
  case class UnusedOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op = ctx => ???
  }
  implicit def intToByte(n: Int) = n.toByte

  abstract class BaseOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op: VmThread => Unit
  }

}
