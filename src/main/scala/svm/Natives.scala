package svm

object Natives {
  object Route{
    def apply(a: (String, Route)*) = Node(a.toMap)
  }
  trait Route{
    def lookup(s: String): Option[Any]
  }
  case class Node(children: Map[String, Route]) extends Route{
    def lookup(s: String) = {
      val Array(first, rest) =
        if (s.indexOf('/') != -1 && s.indexOf('/') < s.indexOf('(')){
          s.split("/", 2)
        }else{
          Array(s, "")
        }
      children.get(first).flatMap(_.lookup(rest))
    }
  }

  case class Leaf(f: Any) extends Route{
    def lookup(s: String) = {
      if (s == "") Some(f)
      else None
    }
  }

  implicit class pimpedMap(s: String){
    def /(a: (String, Route)*) = s -> Node(a.toMap)
    def -(a: Any) = s -> Leaf(a)
  }
  val noOp = () => ()
  val primitiveMap = Map(
    "float" -> "java/lang/Float",
    "int" -> "java/lang/Integer",
    "double" -> "java/lang/Double"
  )

  def nativeX(getClassFor: String => Class) = Route(
    "java"/(
      "lang"/(
        "Class"/(
          "registerNatives()V"-noOp,
          "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"-((s: svm.Object) => (new Object(getClassFor(primitiveMap(Object.fromVirtual(s).asInstanceOf[String])), getClassFor))),
          "getClassLoader0()Ljava/lang/ClassLoader;"-(() => null),
          "desiredAssertionStatus0(Ljava/lang/Class;)Z"-((x: Any) => 0)
          ),
        "Double"/(
          "doubleToRawLongBits(D)J"-{ java.lang.Double.doubleToRawLongBits(_: Double)}
          ),
        "Float"/(
          "intBitsToFloat(I)F"-{ java.lang.Float.intBitsToFloat(_: Int)},
          "floatToRawIntBits(F)I"-{ java.lang.Float.floatToIntBits(_: Float)}
          ),
        "Object"/(
          "registerNatives()V"-noOp
          ),
        "System"/(
          "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"-((src: Any, srcPos: Int, dest: Any, destPos: Int, length: Int) =>
            System.arraycopy(src, srcPos, dest, destPos, length)
            ),
          "currentTimeMillis()J"-(() => System.currentTimeMillis()),
          "nanoTime()J"-(() => System.nanoTime()),
          "identityHashCode(Ljava/lang/Object;)I"-((x: svm.Object) => System.identityHashCode(x)),
          "registerNatives()V"-noOp
          )
        )
      )
  )
}
