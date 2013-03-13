package sm.imm

import sm.VmThread
import sm.virt
package object opcodes {
  def ext(x: virt.Val): virt.Val = {
    x match {
      case b: virt.Boolean => if (b) 1 else 0
      case c: virt.Char => c.toInt
      case b: virt.Byte => b.toInt
      case s: virt.Short => s.toInt
      case x => x
    }
  }
  object Intish{
    def unapply(x: virt.Val): Option[virt.Int] = x match{
      case b: virt.Boolean => Some(if (b) 1 else 0 )
      case c: virt.Char => Some(c.toInt)
      case b: virt.Byte => Some(b.toInt)
      case s: virt.Short => Some(s.toInt)
      case i: virt.Int => Some(i)
      case _ => None
    }
  }
  object Cat1{
    def unapply(x: virt.Val): Option[virt.Val] = x match{
      case b: virt.Boolean => Some(b)
      case c: virt.Char => Some(c)
      case b: virt.Byte => Some(b)
      case s: virt.Short => Some(s)
      case i: virt.Int => Some(i)
      case f: virt.Float => Some(f)
      case a: virt.Arr => Some(a)
      case o: virt.Obj => Some(o)
    }
  }
  object Cat2{
    def unapply(x: virt.Val): Option[virt.Val] = x match{
      case l: virt.Long => Some(l)
      case d: virt.Double => Some(d)
      case _ => None

    }
  }
  case class UnusedOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op = ctx => ???
  }
  implicit def intToByte(n: Int) = n.toByte
  implicit class poppable(val vt: VmThread) extends AnyVal{
    def pop = vt.frame.stack.pop()
    def push(x: virt.Val) = vt.frame.stack.push(x)
  }
  abstract class BaseOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op: VmThread => Unit
  }

}
