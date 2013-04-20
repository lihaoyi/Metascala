import metascala.rt.Thread
import collection.mutable

package object metascala {
  private[metascala] implicit class castable(val x: Any) extends AnyVal{
    def cast[T] = x.asInstanceOf[T]
  }

  implicit class pimpedVal(v: Val){
    def isObj(implicit vm: VM) = vm.Heap(v) < 0
    def isArr(implicit vm: VM) = vm.Heap(v) >= 0
    def obj(implicit vm: VM) = {
      assert(v != 0)
      new vrt.Obj(v)
    }
    def arr(implicit vm: VM) = {
      assert(v != 0)
      new vrt.Arr(v)
    }
  }
  object Val{
    val Null = 0
    implicit def objToVal(x: vrt.Obj) = x.address
    implicit def arrToVal(x: vrt.Arr) = x.address
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

  type Z = Boolean
  object Z extends Prim[Boolean]{
    def apply(x: Val) = x != 0
    def read(x: => Val) = this(x)
    def write(x: Boolean, out: Val => Unit) = out(if (x) 1 else 0)
    def size = 1
    def boxedClass = classOf[java.lang.Boolean]
    def primClass = classOf[Boolean]
  }

  type B = Byte
  object B extends Prim[Byte]{
    def apply(x: Val) = x.toByte
    def read(x: => Val) = this(x)
    def write(x: Byte, out: Val => Unit) = out(x)
    def size = 1
    def boxedClass = classOf[java.lang.Byte]
    def primClass = classOf[Byte]
  }

  type C = Char
  object C extends Prim[Char]{
    def apply(x: Val) = x.toChar
    def read(x: => Val) = this(x)
    def write(x: Char, out: Val => Unit) = out(x)
    def size = 1
    def boxedClass = classOf[java.lang.Character]
    def primClass = classOf[Char]
  }

  type S = Short
  object S extends Prim[Short]{
    def apply(x: Val) = x.toShort
    def read(x: => Val) = this(x)
    def write(x: Short, out: Val => Unit) = out(x)
    def size = 1
    def boxedClass = classOf[java.lang.Short]
    def primClass = classOf[Short]
  }

  type I = Int
  object I extends Prim[Int]{
    def apply(x: Val) = x
    def read(x: => Val) = this(x)
    def write(x: Int, out: Val => Unit) = out(x)
    def size = 1
    def boxedClass = classOf[java.lang.Integer]
    def primClass = classOf[Integer]
  }

  type F = Float
  object F extends Prim[Float]{
    def apply(x: Val) = java.lang.Float.intBitsToFloat(x)
    def read(x: => Val) = this(x)
    def write(x: Float, out: Val => Unit) = out(java.lang.Float.floatToRawIntBits(x))
    def size = 1
    def boxedClass = classOf[java.lang.Float]
    def primClass = classOf[Float]
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
    def size = 2
    def boxedClass = classOf[java.lang.Long]
    def primClass = classOf[Long]
  }

  type D = Double
  object D extends Prim[Double]{
    def apply(v1: Val, v2: Val) = java.lang.Double.longBitsToDouble(J(v1, v2))
    def read(x: => Val) = java.lang.Double.longBitsToDouble(J.read(x))
    def write(x: Double, out: Val => Unit) = J.write(java.lang.Double.doubleToRawLongBits(x), out)
    def size = 2
    def boxedClass = classOf[java.lang.Double]
    def primClass = classOf[Double]
  }

  type Val = Int

  val prims = Map(
    'Z' -> Z,
    'B' -> B,
    'C' -> C,
    'S' -> S,
    'I' -> I,
    'F' -> F,
    'J' -> J,
    'D' -> D
  )
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

  def forNameBoxed(name: String) = {
    name match{
      case x if prims.contains(x(0)) => prims(x(0)).boxedClass
      case x => Class.forName(x)
    }
  }
  def forName(name: String) = {
    name match{
      case x if prims.contains(x(0)) => prims(x(0)).primClass
      case x => Class.forName(x)
    }
  }

  def getAllFields(cls: Class[_]): Seq[java.lang.reflect.Field] = {
    Option(cls.getSuperclass)
      .toSeq
      .flatMap(getAllFields)
      .++(cls.getDeclaredFields)
  }

  implicit def stringToClass(s: String)(implicit vm: VM) = vm.ClsTable(imm.Type.Cls(s))
}
