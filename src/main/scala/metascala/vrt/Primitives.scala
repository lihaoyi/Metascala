package metascala
package vrt

abstract class Prim[T: imm.Type.Prim.Info]{
  def tpe = implicitly[imm.Type.Prim.Info[T]].tpe
}
case class Boolean(v: scala.Boolean) extends Prim[scala.Boolean] with Val
case class Byte(v: scala.Byte) extends Prim[scala.Byte] with Val
case class Char(v: scala.Char) extends Prim[scala.Char] with Val
case class Short(v: scala.Short) extends Prim[scala.Short] with Val
case class Int(v: scala.Int) extends Prim[scala.Int] with Cat1
case class Float(v: scala.Float)extends Prim[scala.Float] with Cat1
case class Long(v: scala.Long) extends Prim[scala.Long] with Cat2
case class Double(v: scala.Double) extends Prim[scala.Double] with Cat2
case object Null extends Cat1{
  def tpe = ???
}