package svm
package imm

import scala.Array
import virt._


object Type{
  object Primitives{
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
  }



  val primitiveMap = Map(
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
      case "Z" | "B" | "C" | "S" | "I" | "J" | "F" | "D" | "V" => Prim.read(s)
      case s if s.startsWith("L") && s.endsWith(";") => Cls.read(s.drop(1).dropRight(1))
      case s if s.startsWith("[") => Arr.read(s)
      case s if s.startsWith("(") => Desc.read(s)
      case s => Cls.read(s)
    }
  }
  object Arr{
    def read(s: String) = Arr(Type.read(s.drop(1)))
  }
  case class Arr(innerType: Type) extends Type{
    def unparse = "[" + innerType.unparse
    def name = "["
    def cls(implicit vm: VM) = vm.Types(this)

    def realCls = ???
  }
  object Cls{
    def read(s: String) = Cls(s)
  }
  case class Cls(name: String) extends Type{
    def unparse = name
    def cls(implicit vm: VM) = vm.Classes(this)
    def realCls = classOf[virt.Obj]
    override def obj(implicit vm: VM): virt.Type = vm.Types(this)
  }

  object Prim{
    def read(s: String) = Prim(s)
  }
  case class Prim(name: String) extends Type{
    def unparse = name
    def clsType = Cls(primitiveMap(shortMap(name)))
    def realCls = Primitives.fromChar(name(0))
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
  case class Desc(args: Seq[Type], ret: Type) extends Type{
    def name = unparse
    def unparse = "(" + args.map(Desc.unparse).foldLeft("")(_+_) + ")" + Desc.unparse(ret)
    def cls(implicit vm: VM) = ???
    def realCls = ???
  }
}
trait Type{
  def unparse: String
  def name: String
  def realCls: Class[_]
  def obj(implicit vm: VM): virt.Type = vm.Types(this)
}


