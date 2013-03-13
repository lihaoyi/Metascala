package sm
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

  def default(desc: imm.Type) = {

    desc match{
      case imm.Type.Prim("B") => 0: Byte
      case imm.Type.Prim("C") => 0: Char
      case imm.Type.Prim("I") => 0
      case imm.Type.Prim("J") => 0L
      case imm.Type.Prim("F") => 0F
      case imm.Type.Prim("D") => 0D
      case imm.Type.Prim("S") => 0: Short
      case imm.Type.Prim("Z") => false
      case _ => null
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
    def read(s: String) = Arr(Type.read(s.drop(1)).asInstanceOf[Entity])
  }
  case class Arr(innerType: Type.Entity) extends Entity{
    def unparse = "[" + innerType.unparse
    def name = "["
    def parent(implicit vm: VM) = Some(imm.Type.Cls("java/lang/Object"))
    def realCls = innerType.realCls
  }
  object Cls{
    def read(s: String) = Cls(s)
  }
  case class Cls(name: String) extends Entity{
    def unparse = name
    def cls(implicit vm: VM) = vm.Classes(this)
    def parent(implicit vm: VM) = this.cls.clsData.superType
    override def obj(implicit vm: VM): virt.Type = vm.Types(this)
    def realCls = classOf[Object]
  }

  object Prim{
    def read(s: String) = Prim(s)
  }
  case class Prim(name: String) extends Entity{
    def unparse = name
    def clsType = Cls(primitiveMap(shortMap(name)))
    def realCls = Primitives.fromChar(name(0))

    def parent(implicit vm: VM) = ???
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
  }
  trait Entity extends Type{
    def parent(implicit vm: VM): Option[Entity]
    def realCls: Class[_]
  }
}
trait Type{
  def unparse: String
  def name: String

  def obj(implicit vm: VM): virt.Type = vm.Types(this)
}


