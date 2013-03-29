package sm
package imm

import reflect.ClassTag

object Type{
  def read(s: String): Type = {
    s match{
      case "Z" | "B" | "C" | "S" | "I" | "J" | "F" | "D" | "V" => Prim.read(s)
      case s if s.startsWith("L") && s.endsWith(";") => Cls.read(s.drop(1).dropRight(1))
      case s if s.startsWith("[") => Arr.read(s)
      case s if s.startsWith("(") => Desc.read(s)
      case s => Cls.read(s)
    }
  }
  trait Ref extends Entity
  object Arr{
    def read(s: String) = Arr(Type.read(s.drop(1)).asInstanceOf[Entity])
  }

  case class Arr(innerType: Type.Entity) extends Ref{
    def unparse = "[" + innerType.unparse
    def name = "[" + innerType.unparse
    def parent(implicit vm: VM) = Some(imm.Type.Cls("java/lang/Object"))
    def realCls = innerType.realCls
    def default = vrt.Null
  }
  object Cls{
    def read(s: String) = Cls(s)
  }
  case class Cls(name: String) extends Ref{
    assert(!name.contains('.'), "Cls name cannot contain . " + name)
    def unparse = name
    def cls(implicit vm: VM) = vm.Classes(this)
    def parent(implicit vm: VM) = this.cls.clsData.superType
    override def obj(implicit vm: VM): vrt.Type = vm.Types(this)
    def realCls = classOf[Object]
    def default = vrt.Null
  }

  object Prim{
    def read(s: String) = Prim(s(0))
    class Info[T: ClassTag](val tpe: imm.Type.Prim,
                            val name: String,
                            val boxName: String,
                            defaultV: => T,
                            val constructor: T => vrt.Val){

      lazy val default = constructor(defaultV)
      val realCls: Class[_] = implicitly[ClassTag[T]].getClass
      def newArray(n: Int): Array[T] = new Array[T](n)
      def newVirtArray(n: Int): vrt.Arr.Prim[T] = new vrt.Arr.Prim(new Array[T](n))(this)

    }
    object Info{
      private[this] implicit def char2Type(c: Char) = imm.Type.Prim(c)
      implicit val ZC = new Info[Boolean]( 'Z', "boolean", "java/lang/Boolean",  false,  vrt.Boolean)
      implicit val BC = new Info[Byte](    'B', "byte",    "java/lang/Byte",     0,      vrt.Byte)
      implicit val CC = new Info[Char](    'C', "char",    "java/lang/Character",0,      vrt.Char)
      implicit val SC = new Info[Short](   'S', "short",   "java/lang/Short",    0,      vrt.Short)
      implicit val IC = new Info[Int](     'I', "int",     "java/lang/Integer",  0,      vrt.Int)
      implicit val FC = new Info[Float](   'F', "float",   "java/lang/Float",    0,      vrt.Float)
      implicit val JC = new Info[Long](    'J', "long",    "java/lang/Long",     0,      vrt.Long)
      implicit val DC = new Info[Double](  'D', "double",  "java/lang/Double",   0,      vrt.Double)
      implicit val VC = new Info[Void](    'V', "void",    "java/lang/Void",     ???,    x => ???)
      val all: Seq[Info[_]] = Seq(ZC, BC, CC, SC, IC, FC, JC, DC, VC)
      val charMap = all.map(x => x.tpe.char -> (x: Info[_])).toMap[Char, Info[_]]

      def apply[T: Info]() = implicitly[Info[T]].tpe
    }
  }
  case class Prim(char: Char) extends Entity{
    def unparse = ""+char
    def name = imm.Type.Prim.Info.charMap(char).name

    def realCls = imm.Type.Prim.Info.charMap(name(0)).realCls

    def parent(implicit vm: VM) = ???

    def default = Prim.Info.charMap(char).default
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
    def unparse = "(" + args.map(Desc.unparse).foldLeft("")(_+_) + ")" + Desc.unparse(ret)
  }
  trait Entity extends Type{
    def parent(implicit vm: VM): Option[Entity]
    def realCls: Class[_]
    // byte char int long java/lang/String
    def name: String
    def default: vrt.Val

  }
}
trait Type{
  //  B C I J Ljava/lang/String;
  def unparse: String

  def obj(implicit vm: VM): vrt.Type = vm.Types(this)
}


