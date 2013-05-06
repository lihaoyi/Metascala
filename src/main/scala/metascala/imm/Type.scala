package metascala
package imm

import reflect.ClassTag


object Type{
  def read(s: String): Type = {
    s match{
      case "Z" | "B" | "C" | "S" | "I" | "J" | "F" | "D" | "V" => Prim.read(s)
      case s if s.startsWith("L") && s.endsWith(";") => Cls.read(s.drop(1).dropRight(1))
      case s if s.startsWith("[") => Arr.read(s)
      case s => Cls.read(s)
    }
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
  }

  /**
   * Array Types
   * @param innerType The type of the components of the array
   */
  case class Arr(innerType: Type) extends Ref{
    def size = 1
    def unparse = "[" + innerType.unparse
    def name = "[" + innerType.unparse
    def parent(implicit vm: VM) = Some(imm.Type.Cls("java/lang/Object"))
    def realCls = innerType.realCls
    def methodType = Type.Cls("java/lang/Object")
    def prim = I
  }
  object Cls{
    def read(s: String) = Cls(s)
  }

  /**
   * Class Types
   * @param name the fuly qualified name of the class
   */
  case class Cls(name: String) extends Ref {
    //assert(!name.contains('.'), "Cls name cannot contain . " + name)
    def size = 1
    def unparse = name
    def cls(implicit vm: VM) = vm.ClsTable(this)
    def parent(implicit vm: VM) = this.cls.clsData.superType
    def realCls = classOf[Object]
    def methodType: Type.Cls = this
    def prim = I
    override val hashCode = name.hashCode
  }

  object Prim{
    def read(s: String) = Prim(s(0))
    class Info(val name: String,
               val boxName: String)
    val info = Map(
      'Z' -> new Info("boolean", "java/lang/Boolean"  ),
      'B' -> new Info("byte",    "java/lang/Byte"     ),
      'C' -> new Info("char",    "java/lang/Character"),
      'S' -> new Info("short",   "java/lang/Short"    ),
      'I' -> new Info("int",     "java/lang/Integer"  ),
      'F' -> new Info("float",   "java/lang/Float"    ),
      'L' -> new Info("long",    "java/lang/Long"     ),
      'D' -> new Info("double",  "java/lang/Double"   ),
      'V' -> new Info("void",    "java/lang/Void"     )
    )
  }

  /**
   * Primitive Types
   */
  case class Prim(char: Char) extends Type{
    def size = if (char == 'D' || char == 'J') 2 else 1
    def info = Prim.info(char)
    def unparse = ""+char
    def name = info.name

    def realCls = Class.forName(info.boxName.replace('/', '.'))

    def parent(implicit vm: VM) = ???
    def prim = metascala.Prim.all(char)
  }


}
/**
 * Represents all variable types within the Metascala VM
 */
trait Type{
  /**
   * Converts this object into a nice, human readable string
   */
  def unparse: String
  override def toString = unparse
  /**
   * Retrieves the Class object in the host JVM which represents the
   * given Type inside the Metascala VM
   */
  def realCls: Class[_]

  def size: Int
  def name: String
  def prim: Prim[_]


}

object Desc{
  def read(s: String) = {
    val scala.Array(argString, ret) = s.drop(1).split(')')
    var args = Seq[String]()
    var index = 0
    while(index < argString.length){
      val firstChar = argString.indexWhere(x => "BCDFIJSZL".contains(x), index)
      val split = argString(firstChar) match{
        case 'L' => argString.indexWhere(x => ";".contains(x), index)
        case _ => argString.indexWhere(x => "BCDFIJSZ".contains(x), index)
      }

      args = args :+ argString.substring(index, split+1)
      index = split +1
    }
    Desc(args.map(Type.read), Type.read(ret))
  }
  def unparse(t: Type): String = {
    t match{
      case t: Type.Cls => "L" + t.unparse + ";"
      case t: Type.Arr => "[" + unparse(t.innerType)
      case x => x.unparse
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
    val longArgSize = args.count(x => x == Type.Prim('J') || x == Type.Prim('D'))

    baseArgSize + longArgSize
  }
  override def toString = unparse
}
