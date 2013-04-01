package sm.vrt


case class Boolean(v: scala.Boolean) extends Val
case class Byte(v: scala.Byte) extends Val
case class Char(v: scala.Char) extends Val
case class Short(v: scala.Short) extends Val
case class Int(v: scala.Int) extends Cat1
case class Float(v: scala.Float)extends Cat1
case class Long(v: scala.Long) extends Cat2
case class Double(v: scala.Double) extends Cat2
case object Null extends Cat1
case object Unit extends Cat1