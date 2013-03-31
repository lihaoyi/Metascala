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


trait StackVal extends Val{
  def size: scala.Int
}
trait Cat1 extends StackVal{
  def size: scala.Int = 1
}
trait Cat2 extends StackVal{
  def size: scala.Int = 2
}