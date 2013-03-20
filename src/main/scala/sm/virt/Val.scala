package sm.virt

import sm.virt

trait Val
object Val{
  implicit class stackable(s: virt.Val){
    def toStackVal = s match {
      case virt.Boolean(b) => virt.Int(if (b) 1 else 0)
      case virt.Char(c) => virt.Int(c)
      case virt.Byte(b) => virt.Int(b)
      case virt.Short(s) => virt.Int(s)
      case x: virt.StackVal => x
    }
  }
}

trait StackVal extends Val
trait Cat1{this: StackVal => }
trait Cat2{this: StackVal => }