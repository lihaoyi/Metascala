package metascala
package imm
import collection.mutable
import reflect.ClassTag


object Type{
  def read(s: String): Type = s match{
    case x if Prim.all.contains(x(0)) => Prim.all(x(0))
    case s if s.startsWith("L") && s.endsWith(";") => Cls.read(s.drop(1).dropRight(1))
    case s if s.startsWith("[") => Arr.read(s)
    case s => Cls.read(s)
  }

  def readJava(s: String): Type = s match {
    case x if Prim.allJava.contains(x) => Prim.allJava(x)
    case s if s.startsWith("[") => Arr.readJava(s)
    case s => Cls.readJava(s)
  }

  /**
   * Reference types, which can either be Class or Array types
   */
  trait Ref extends Type{
    def methodType: Type.Cls
    def parent(implicit vm: VM): Option[Type]
  }
  object Arr{
    def read(s: String) = Arr(Type.read(s.drop(1)))
    def readJava(s: String) = Arr(s.drop(1) match {
      case x if Prim.all.contains(x(0)) => Prim.all(x(0))
      case x if x.startsWith("L") => Cls.read(x.drop(1).dropRight(1).toSlash)
      case x => Type.readJava(x)
    })
  }

  /**
   * Array Types
   * @param innerType The type of the components of the array
   */
  case class Arr(innerType: Type) extends Ref{
    def size = 1
    def name = "[" + innerType.name
    def parent(implicit vm: VM) = Some(imm.Type.Cls("java/lang/Object"))
    def realCls = innerType.realCls
    def methodType = Type.Cls("java/lang/Object")
    def prettyRead(x: () => Val) = "[" + innerType.shortName + "#" + x()
    def javaName = innerType match{
      case tpe: Cls => "[L" + tpe.javaName + ";"
      case tpe: Prim[_] => "[" + tpe.internalName
      case tpe => "[" + tpe.javaName
    }
    def internalName = "[" + innerType.internalName
  }
  object Cls{
    def read(s: String) = Cls(s)
    def readJava(s: String) = Cls(s.toSlash)
  }

  /**
   * Class Types
   * @param name the fuly qualified name of the class
   */
  case class Cls(name: String) extends Ref {
    assert(!name.contains('.'), "Cls name cannot contain . " + name)
    assert(!name.contains('['), "Cls name cannot contain [ " + name)
    def size = 1
    def cls(implicit vm: VM) = vm.ClsTable(this)
    def parent(implicit vm: VM) = this.cls.superType
    def realCls = classOf[Object]
    def methodType: Type.Cls = this

    override val hashCode = name.hashCode
    def prettyRead(x: () => Val) = shorten(name) + "#" + x()
    def internalName = "L" + name + ";"
    def javaName = name.replace('/', '.')
  }

  abstract class Prim[T: ClassTag](val size: Int, val javaName: String) extends imm.Type{
    def read(x: () => Val): T
    def write(x: T, out: Val => Unit): Unit
    def boxedClass: Class[_]
    val primClass: Class[_] = implicitly[ClassTag[T]].runtimeClass
    def realCls = Class.forName(boxedClass.getName.replace('/', '.'))
    def prim = this
    def productPrefix: String
    def name = productPrefix
    def internalName = name
    def prettyRead(x: () => Val) = toString + "#" + read(x)
  }
  object Prim extends {
    def read(s: String) = all(s(0))
    val all: Map[Char, Prim[_]] = Map(
      'V' -> (V: Prim[_]),
      'Z' -> (Z: Prim[_]),
      'B' -> (B: Prim[_]),
      'C' -> (C: Prim[_]),
      'S' -> (S: Prim[_]),
      'I' -> (I: Prim[_]),
      'F' -> (F: Prim[_]),
      'J' -> (J: Prim[_]),
      'D' -> (D: Prim[_])
    )

    val allJava: Map[String, Prim[_]] = Map(
      "void" -> (V: Prim[_]),
      "boolean" -> (Z: Prim[_]),
      "byte" -> (B: Prim[_]),
      "char" -> (C: Prim[_]),
      "short" -> (S: Prim[_]),
      "int" -> (I: Prim[_]),
      "float" -> (F: Prim[_]),
      "long" -> (J: Prim[_]),
      "double" -> (D: Prim[_])
    )
    
    def unapply(p: Prim[_]) = Some(p.javaName)

    case object V extends Prim[Unit](0, "void"){
      def apply(x: Val) = ???
      def read(x: () => Val) = ()
      def write(x: Unit, out: Val => Unit) = ()
      def boxedClass = classOf[java.lang.Void]

    }
    type Z = Boolean
    case object Z extends Prim[Boolean](1, "boolean"){
      def apply(x: Val) = x != 0
      def read(x: () => Val) = this(x())
      def write(x: Boolean, out: Val => Unit) = out(if (x) 1 else 0)
      def boxedClass = classOf[java.lang.Boolean]
    }
    type B = Byte
    case object B extends Prim[Byte](1, "byte"){
      def apply(x: Val) = x.toByte
      def read(x: () => Val) = this(x())
      def write(x: Byte, out: Val => Unit) = out(x)
      def boxedClass = classOf[java.lang.Byte]
    }
    type C = Char
    case object C extends Prim[Char](1, "char"){
      def apply(x: Val) = x.toChar
      def read(x: () => Val) = this(x())
      def write(x: Char, out: Val => Unit) = out(x)
      def boxedClass = classOf[java.lang.Character]
    }
    type S = Short
    case object S extends Prim[Short](1, "short"){
      def apply(x: Val) = x.toShort
      def read(x: () => Val) = this(x())
      def write(x: Short, out: Val => Unit) = out(x)
      def boxedClass = classOf[java.lang.Short]
    }
    type I = Int
    case object I extends Prim[Int](1, "int"){
      def apply(x: Val) = x
      def read(x: () => Val) = this(x())
      def write(x: Int, out: Val => Unit) = out(x)
      def boxedClass = classOf[java.lang.Integer]
    }
    type F = Float
    case object F extends Prim[Float](1, "float"){
      def apply(x: Val) = java.lang.Float.intBitsToFloat(x)
      def read(x: () => Val) = this(x())
      def write(x: Float, out: Val => Unit) = out(java.lang.Float.floatToRawIntBits(x))
      def boxedClass = classOf[java.lang.Float]
    }
    type J = Long
    case object J extends Prim[Long](2, "long"){
      def apply(v1: Val, v2: Val) = v1.toLong << 32 | v2 & 0xFFFFFFFFL
      def read(x: () => Val) = {
        this(x(), x())
      }
      def write(x: Long, out: Val => Unit) = {
        out((x >> 32).toInt)
        out(x.toInt)
      }
      def boxedClass = classOf[java.lang.Long]
    }
    type D = Double
    case object D extends Prim[Double](2, "double"){
      def apply(v1: Val, v2: Val) = java.lang.Double.longBitsToDouble(J(v1, v2))
      def read(x: () => Val) = java.lang.Double.longBitsToDouble(J.read(x))
      def write(x: Double, out: Val => Unit) = J.write(java.lang.Double.doubleToRawLongBits(x), out)
      def boxedClass = classOf[java.lang.Double]
    }
  }


}
/**
 * Represents all variable types within the Metascala VM
 */
trait Type{
  /**
   * The JVMs internal representation
   * - V Z B C S I F J D
   * - Ljava/lang/Object; [Ljava/lang/String;
   */
  def internalName: String

  /**
   * Nice name to use for most things
   * - V Z B C S I F J D
   * - java/lang/Object [java/lang/String
   */
  def name: String

  override def toString = name

  /**
   * A human-friendly name for this type that can be used everywhere without
   * cluttering the screen.
   * - V Z B C S I F J D
   * - j/l/Object [j/l/String
   */
  def shortName = shorten(name)

  /**
   * The thing that's returned by Java's getName method
   * - void boolean byte char short int float long double
   * - java.lang.Object [java.lang.String;
   */
  def javaName: String
  /**
   * Retrieves the Class object in the host JVM which represents the
   * given Type inside the Metascala VM
   */
  def realCls: Class[_]

  /**
   * Reads an object of this type from the given input stream into a readable
   * representation
   */
  def prettyRead(x: () => Val): String

  /**
   * 0, 1 or 2 for void, most things and double/long
   */
  def size: Int



  def isRef: Boolean = this.isInstanceOf[imm.Type.Ref]

}

object Desc{
  def read(s: String) = {
    val scala.Array(argString, ret) = s.drop(1).split(')')
    val args = mutable.Buffer.empty[String]
    var index = 0
    while(index < argString.length){
      val firstChar = argString.indexWhere(x => "BCDFIJSZL".contains(x), index)
      val split = argString(firstChar) match{
        case 'L' => argString.indexWhere(x => ";".contains(x), index)
        case _ => argString.indexWhere(x => "BCDFIJSZ".contains(x), index)
      }

      args.append(argString.substring(index, split+1))
      index = split +1
    }
    Desc(args.map(Type.read), Type.read(ret))
  }
  def unparse(t: Type): String = {
    t match{
      case t: Type.Cls => t.name
      case t: Type.Arr => "[" + unparse(t.innerType)
      case x => x.name
    }
  }
}

/**
 * Represents the signature of a method.
 */
case class Desc(args: Seq[Type], ret: Type){
  def unparse = "(" + args.map(Desc.unparse).foldLeft("")(_+_) + ")" + Desc.unparse(ret)
  def argSize = {
    val baseArgSize = args.length
    val longArgSize = args.count(x => x == Type.Prim.J || x == Type.Prim.D)

    baseArgSize + longArgSize
  }
  override def toString = unparse
  def shortName = "(" + args.map(Desc.unparse).map(shorten).foldLeft("")(_+_) + ")" + shorten(Desc.unparse(ret))
}
