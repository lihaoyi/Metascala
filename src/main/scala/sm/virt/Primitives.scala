package sm.virt

trait WrapVal[T]{
  def v: T
}
case class Boolean(v: scala.Boolean) extends WrapVal[scala.Boolean] with Val
case class Byte(v: scala.Byte) extends WrapVal[scala.Byte] with Val
case class Char(v: scala.Char) extends WrapVal[scala.Char] with Val
case class Short(v: scala.Short) extends WrapVal[scala.Short] with Val
case class Int(v: scala.Int) extends WrapVal[scala.Int] with StackVal with Cat1
case class Float(v: scala.Float) extends WrapVal[scala.Float] with StackVal with Cat1
case class Long(v: scala.Long) extends WrapVal[scala.Long] with StackVal with Cat2
case class Double(v: scala.Double) extends WrapVal[scala.Double] with StackVal with Cat2
case object Null extends WrapVal[scala.Null] with StackVal with Cat1{
  def v = null
}
case object Unit extends WrapVal[scala.Unit] with StackVal with Cat1{
  def v = ()
}