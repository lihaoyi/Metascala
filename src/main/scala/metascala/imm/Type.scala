package metascala
package imm
import metascala.util.Agg

import collection.mutable
import reflect.ClassTag


object Type{
  implicit def strClsTypeShorthand(name: String): Cls = Cls(name)

  def read(s: String): Type = s match{
    case x if Prim.all.contains(x(0)) => Prim.all(x(0))
    case s if s.startsWith("L") && s.endsWith(";") => Cls.apply(s.drop(1).dropRight(1))
    case s if s.startsWith("[") => Arr.read(s)
    case s => Cls.apply(s)
  }

  def readJava(s: String): Type = s match {
    case x if Prim.allJava.contains(x) => Prim.allJava(x)
    case s if s.startsWith("[") => Arr.readJava(s)
    case s => Cls.apply(s)
  }

  /**
   * Reference types, which can either be Class or Array types
   */
  trait Ref extends Type
  object Arr{
    def read(s: String) = Arr(Type.read(s.drop(1)))
    def readJava(s: String) = Arr(s.drop(1) match {
      case x if Prim.all.contains(x(0)) => Prim.all(x(0))
      case x if x.startsWith("L") => Cls.apply(x.drop(1).dropRight(1))
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
    def realCls = innerType.realCls
    def javaName = innerType match{
      case tpe: Cls => "[L" + tpe.javaName + ";"
      case tpe: Prim[_] => "[" + tpe.internalName
      case tpe => "[" + tpe.javaName
    }
    def internalName = "[" + innerType.internalName
  }
  object Cls{
    implicit def apply(name: String): Cls = new Cls(name.replace('.', '/'))
    def unapply(cls: Cls): Option[String] = Some(cls.name)
  }

  /**
   * Class Types
   * @param name the fuly qualified name of the class
   */
  class Cls private (val name: String) extends Ref {
    def size = 1

    def realCls = classOf[Object]

    override val hashCode = name.hashCode
    override def equals(other: Any) = other match{
      case o: Cls => o.name == name
      case _ => false
    }
    override def toString = s"Cls($name)"
    def internalName = "L" + name + ";"
    def javaName = name.replace('/', '.')
  }

  abstract class Prim[T: ClassTag](val size: Int,
                                   val index: Int,
                                   val javaName: String) extends imm.Type{
    def read(x: () => Int): T
    def write(x: T, out: Int => Unit): Unit
    def boxedClass: Class[_]
    val primClass: Class[_] = implicitly[ClassTag[T]].runtimeClass
    def realCls = Class.forName(boxedClass.getName.replace('/', '.'))

    def productPrefix: String
    def name = productPrefix
    def internalName = name
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
    val indexed: IndexedSeq[Prim[_]] = {
      val arr = new Array[Prim[_]](all.size)
      for (prim <- all.values) arr(prim.index) = prim
      for(v <- arr) assert(v != null)
      arr
    }


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

    implicit case object V extends Prim[Unit](0, 0, "void"){
      def apply(x: Int) = ???
      def read(x: () => Int) = ()
      def write(x: Unit, out: Int => Unit) = ()
      def boxedClass = classOf[java.lang.Void]

    }
    type Z = Boolean
    implicit case object Z extends Prim[Boolean](1, 1, "boolean"){
      def apply(x: Int) = x != 0
      def read(x: () => Int) = this(x())
      def write(x: Boolean, out: Int => Unit) = out(if (x) 1 else 0)
      def boxedClass = classOf[java.lang.Boolean]
    }
    type B = Byte
    implicit case object B extends Prim[Byte](1, 2, "byte"){
      def apply(x: Int) = x.toByte
      def read(x: () => Int) = this(x())
      def write(x: Byte, out: Int => Unit) = out(x)
      def boxedClass = classOf[java.lang.Byte]
    }
    type C = Char
    implicit case object C extends Prim[Char](1, 3, "char"){
      def apply(x: Int) = x.toChar
      def read(x: () => Int) = this(x())
      def write(x: Char, out: Int => Unit) = out(x)
      def boxedClass = classOf[java.lang.Character]
    }
    type S = Short
    implicit case object S extends Prim[Short](1, 4, "short"){
      def apply(x: Int) = x.toShort
      def read(x: () => Int) = this(x())
      def write(x: Short, out: Int => Unit) = out(x)
      def boxedClass = classOf[java.lang.Short]
    }
    type I = Int
    implicit case object I extends Prim[Int](1, 5, "int"){
      def apply(x: Int) = x
      def read(x: () => Int) = this(x())
      def write(x: Int, out: Int => Unit) = out(x)
      def boxedClass = classOf[java.lang.Integer]
    }
    type F = Float
    implicit case object F extends Prim[Float](1, 6, "float"){
      def apply(x: Int) = java.lang.Float.intBitsToFloat(x)
      def read(x: () => Int) = this(x())
      def write(x: Float, out: Int => Unit) = out(java.lang.Float.floatToRawIntBits(x))
      def boxedClass = classOf[java.lang.Float]
    }
    type J = Long
    implicit case object J extends Prim[Long](2, 7, "long"){
      def apply(v1: Int, v2: Int) = v1.toLong << 32 | v2 & 0xFFFFFFFFL
      def read(x: () => Int) = {
        this(x(), x())
      }
      def write(x: Long, out: Int => Unit) = {
        out((x >> 32).toInt)
        out(x.toInt)
      }
      def boxedClass = classOf[java.lang.Long]
    }
    type D = Double
    implicit case object D extends Prim[Double](2, 8, "double"){
      def apply(v1: Int, v2: Int) = java.lang.Double.longBitsToDouble(J(v1, v2))
      def read(x: () => Int) = java.lang.Double.longBitsToDouble(J.read(x))
      def write(x: Double, out: Int => Unit) = J.write(java.lang.Double.doubleToRawLongBits(x), out)
      def boxedClass = classOf[java.lang.Double]
    }
  }


}
/**
 * Represents all variable types within the Metascala VM
 */
sealed trait Type{
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
    Desc(Agg.from(args.map(Type.read)), Type.read(ret))
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
case class Desc(args: Agg[Type], ret: Type){
  def unparse = "(" + args.map(Desc.unparse).foldLeft("")(_+_) + ")" + Desc.unparse(ret)
  override def toString = unparse
}
