package svm.model


object TypeDesc{

  type B = Byte
  type C = Char
  type I = Int
  type J = Long
  type F = Float
  type D = Double
  type S = Short
  type Z = Boolean
  type L = Object


  def fromChar(c: Char) = c match{
    case 'B' => classOf[B]
    case 'C' => classOf[C]
    case 'I' => classOf[I]
    case 'J' => classOf[J]
    case 'F' => classOf[F]
    case 'D' => classOf[D]
    case 'S' => classOf[S]
    case 'Z' => classOf[Z]
    case 'L' => classOf[L]
  }
  def read(s: String): TypeDesc = {
    val Array(argString, ret) = s.drop(1).split("\\)")
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
    TypeDesc(args.map(Type.read), Type.read(ret))
  }



}

case class TypeDesc(args: Seq[Type], ret: Type){
  def unparse = "(" + args.map(_.unparse).foldLeft("")(_+_) + ")" + ret.unparse

}

object Type{val primitiveMap = Map(
  "boolean" -> "java/lang/Boolean",
  "byte" -> "java/lang/Byte",
  "char" -> "java/lang/Character",
  "short" -> "java/lang/Short",
  "int" -> "java/lang/Integer",
  "long" -> "java/lang/Long",
  "float" -> "java/lang/Float",
  "double" -> "java/lang/Double",
  "void" -> "java/lang/Void"
)
  val shortMap = (_: String) match {
    case "Z" => "boolean"
    case "B" => "byte"
    case "C" => "char"
    case "S" => "short"
    case "I" => "int"
    case "J" => "long"
    case "F" => "float"
    case "D" => "double"
    case "V" => "void"
    case x => x.drop(1).dropRight(1)
  }

  def read(s: String): Type = {
    s match{
      case "Z" | "B" | "C" | "S" | "I" | "J" | "F" | "D" | "V" =>
        Primitive(s)
      case s if s.startsWith("L") && s.endsWith(";") =>
        Class(s.drop(1).dropRight(1))
      case s if s.startsWith("[")=>
        Array(Type.read(s.drop(1)))
    }
  }
  case class Array(innerType: Type) extends Type{
    def unparse = "[" + innerType.unparse
  }
  case class Class(name: String) extends Type{
    def unparse = "L" + name + ";"
  }
  case class Primitive(name: String) extends Type{
    def unparse = name
    def clsType = Class(primitiveMap(shortMap(name)))
  }
}
trait Type{
  def unparse: String
}

