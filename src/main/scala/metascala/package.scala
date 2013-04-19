import metascala.rt.Thread


package object metascala {
  private[metascala] implicit class castable(val x: Any) extends AnyVal{
    def cast[T] = x.asInstanceOf[T]
  }
  trait Vrtable{
    def repr: Val
  }
  implicit class magicBoolean(v: Boolean){
    def repr = if (v) 1 else 0
  }
  implicit class magicChar(v: Char){
    def repr = v.toInt
  }
  implicit class magicFloat(v: Float){
    def repr = java.lang.Float.floatToRawIntBits(v)
  }
  implicit class magicLong(v: Long){
    def repr = ((v >> 32).toInt, v.toInt)
  }
  implicit class magicDouble(v: Double){
    def repr = {
      val x = java.lang.Double.doubleToLongBits(v)
      ((x >> 32).toInt, x.toInt)
    }
  }
  implicit class pimpedVal(v: Val){
    def isObj(implicit vm: VM) = vm.Heap(v) < 0
    def isArr(implicit vm: VM) = vm.Heap(v) >= 0
    def obj(implicit vm: VM) = new vrt.Obj(v)
    def arr(implicit vm: VM) = new vrt.Arr(v)
  }
  object Val{
    val Null = 0
    implicit def objToVal(x: vrt.Obj) = x.address
    implicit def arrToVal(x: vrt.Arr) = x.address
  }
  trait Prim[T]{
    def read(x: => Val): T
    def write(x: T, out: Val => Unit): Unit
  }

  type Z = Boolean
  object Z extends Prim[Boolean]{
    def apply(x: Val) = x != 0
    def read(x: => Val) = this(x)
    def write(x: Boolean, out: Val => Unit) = out(if (x) 1 else 0)
  }

  type B = Byte
  object B extends Prim[Byte]{
    def apply(x: Val) = x.toByte
    def read(x: => Val) = this(x)
    def write(x: Byte, out: Val => Unit) = out(x)
  }

  type C = Char
  object C extends Prim[Char]{
    def apply(x: Val) = x.toChar
    def read(x: => Val) = this(x)
    def write(x: Char, out: Val => Unit) = out(x)
  }

  type S = Short
  object S extends Prim[Short]{
    def apply(x: Val) = x.toShort
    def read(x: => Val) = this(x)
    def write(x: Short, out: Val => Unit) = out(x)
  }

  type I = Int
  object I extends Prim[Int]{
    def apply(x: Val) = x
    def read(x: => Val) = this(x)
    def write(x: Int, out: Val => Unit) = out(x)
  }

  type F = Float
  object F extends Prim[Float]{
    def apply(x: Val) = java.lang.Float.intBitsToFloat(x)
    def read(x: => Val) = this(x)
    def write(x: Float, out: Val => Unit) = out(java.lang.Float.floatToRawIntBits(x))
  }

  type J = Long
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
  }

  type D = Double
  object D extends Prim[Double]{
    def apply(v1: Val, v2: Val) = java.lang.Double.longBitsToDouble(J(v1, v2))
    def read(x: => Val) = java.lang.Double.longBitsToDouble(J.read(x))
    def write(x: Double, out: Val => Unit) = J.write(java.lang.Double.doubleToRawLongBits(x), out)
  }

  type Val = Int

  implicit class poppable(val vt: Thread) extends AnyVal{
    def pop = vt.frame.pop
    def push(x: Val): Unit = vt.frame.push(x)
    def push(x: (Val, Val)): Unit = {
      push(x._1)
      push(x._2)
    }
    def popArgs(n: Int) = {
      val args = new Array[Val](n)
      var i = n-1
      while(i >= 0) {args(i) = vt.frame.pop; i-=1}
      args
    }
  }

  implicit class pimpedString(val s: String){
    def toDot = s.replace('/', '.')
    def toSlash = s.replace('.', '/')
  }
}
