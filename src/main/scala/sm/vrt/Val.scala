package sm.vrt

import sm.vrt

trait Val{

  def toStackVal = this match {
    case vrt.Boolean(b) => vrt.Int(if (b) 1 else 0)
    case vrt.Char(c) => vrt.Int(c)
    case vrt.Byte(b) => vrt.Int(b)
    case vrt.Short(s) => vrt.Int(s)
    case x: vrt.StackVal => x
  }

}


trait StackVal extends Val
trait Cat1{this: StackVal => }
trait Cat2{this: StackVal => }