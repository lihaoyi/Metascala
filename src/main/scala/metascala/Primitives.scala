package metascala

import scala.collection.mutable
object Prim extends {

  val all = Map(
    'Z' -> Z,
    'B' -> B,
    'C' -> C,
    'S' -> S,
    'I' -> I,
    'F' -> F,
    'J' -> J,
    'D' -> D
  )

}
trait Prim[T]{
  def read(x: => Val): T
  def read(x: Seq[Val], index: Int): T = {
    var i = index;
    read{
      i += 1
      x(i - 1)
    }
  }
  def write(x: T, out: Val => Unit): Unit
  def write(x: T, s: mutable.Seq[Val], index: Int): Unit = {
    var i = index;
    write(x, { v =>
      i += 1
      s(i - 1) = v
    })
  }
  def size: Int
  def boxedClass: Class[_]
  def primClass: Class[_]
}

object Z extends Prim[Boolean]{
  def apply(x: Val) = x != 0
  def read(x: => Val) = this(x)
  def write(x: Boolean, out: Val => Unit) = out(if (x) 1 else 0)
  def size = 1
  def boxedClass = classOf[java.lang.Boolean]
  def primClass = classOf[Boolean]
}

object B extends Prim[Byte]{
  def apply(x: Val) = x.toByte
  def read(x: => Val) = this(x)
  def write(x: Byte, out: Val => Unit) = out(x)
  def size = 1
  def boxedClass = classOf[java.lang.Byte]
  def primClass = classOf[Byte]
}

object C extends Prim[Char]{
  def apply(x: Val) = x.toChar
  def read(x: => Val) = this(x)
  def write(x: Char, out: Val => Unit) = out(x)
  def size = 1
  def boxedClass = classOf[java.lang.Character]
  def primClass = classOf[Char]
}

object S extends Prim[Short]{
  def apply(x: Val) = x.toShort
  def read(x: => Val) = this(x)
  def write(x: Short, out: Val => Unit) = out(x)
  def size = 1
  def boxedClass = classOf[java.lang.Short]
  def primClass = classOf[Short]
}

object I extends Prim[Int]{
  def apply(x: Val) = x
  def read(x: => Val) = this(x)
  def write(x: Int, out: Val => Unit) = out(x)
  def size = 1
  def boxedClass = classOf[java.lang.Integer]
  def primClass = classOf[Int]
}

object F extends Prim[Float]{
  def apply(x: Val) = java.lang.Float.intBitsToFloat(x)
  def read(x: => Val) = this(x)
  def write(x: Float, out: Val => Unit) = out(java.lang.Float.floatToRawIntBits(x))
  def size = 1
  def boxedClass = classOf[java.lang.Float]
  def primClass = classOf[Float]
}

object J extends Prim[Long]{
  def apply(v1: Val, v2: Val) = v1.toLong << 32 | v2 & 0xFFFFFFFFL
  def read(x: => Val) = {
    val (a, b) = (x, x)
    this(b, a)
  }
  def write(x: Long, out: Val => Unit) = {
    out((x >> 32).toInt)
    out(x.toInt)
  }
  def size = 2
  def boxedClass = classOf[java.lang.Long]
  def primClass = classOf[Long]
}

object D extends Prim[Double]{
  def apply(v1: Val, v2: Val) = java.lang.Double.longBitsToDouble(J(v1, v2))
  def read(x: => Val) = java.lang.Double.longBitsToDouble(J.read(x))
  def write(x: Double, out: Val => Unit) = J.write(java.lang.Double.doubleToRawLongBits(x), out)
  def size = 2
  def boxedClass = classOf[java.lang.Double]
  def primClass = classOf[Double]
}
