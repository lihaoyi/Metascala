package svm

import annotation.tailrec

object Natives {

  val noOp = () => ()
  val primitiveMap = Map(
    "float" -> "java/lang/Float",
    "int" -> "java/lang/Integer",
    "double" -> "java/lang/Double"
  )

  def nativeX(stackTrace: () => List[StackTraceElement])(implicit getClassFor: String => Class) = Route(
    "java"/(
      "lang"/(
        "Class"/(
          "registerNatives()V"-noOp,
          "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"-((s: svm.Object) => (new Object(getClassFor(primitiveMap(Virtualizer.fromVirtual(s).asInstanceOf[String]))))),
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
          "registerNatives()V"-noOp,
          "getClass()Ljava/lang/Class;" - {() => new ClassObject(stackTrace().head.getClassName) }
        ),
        "System"/(
          "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"-((src: Any, srcPos: Int, dest: Any, destPos: Int, length: Int) =>
            System.arraycopy(src, srcPos, dest, destPos, length)
            ),
          "currentTimeMillis()J"-(() => System.currentTimeMillis()),
          "nanoTime()J"-(() => System.nanoTime()),
          "identityHashCode(Ljava/lang/Object;)I"-((x: svm.Object) => System.identityHashCode(x)),
          "registerNatives()V"-noOp
        ),

        "Throwable"/(
          "fillInStackTrace(I)Ljava/lang/Throwable;" - { (throwable: svm.Object, dummy: Int) =>
            throwable.members(0)("stackTrace") =
              stackTrace().map { f =>
              new svm.Object("java/lang/StackTraceElement",
                "declaringClass" -> f.getClassName,
                "methodName" -> f.getMethodName,
                "fileName" -> f.getFileName,
                "lineNumber" -> f.getLineNumber
              )
            }
            throwable
          }
        )
      )
    )
  )

  object Route{
    def apply(a: (String, Route)*) = Node(a.toMap)
  }
  trait Route{
    def lookup(s: String): Option[Leaf]
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

  implicit def func0(f: () => Any) = (x: Any) => f()
  implicit def func2(f: (Nothing, Nothing) => Any) = f.curried
  implicit def func3(f: (Nothing, Nothing, Nothing) => Any) = f.curried
  implicit def func4(f: (Nothing, Nothing, Nothing, Nothing) => Any) = f.curried
  implicit def func5(f: (Nothing, Nothing, Nothing, Nothing, Nothing) => Any) = f.curried
  case class Leaf(f: Nothing => Any) extends Route{
    def lookup(s: String) = {
      if (s == "") Some(this)
      else None
    }
    def apply(s: Seq[Any]) = {
      @tailrec def rec(f: Any, s: Seq[Any]): Any = (f, s) match {
        case (f1: Function1[Any, Any], head :: tail) => rec(f1(head), tail)
        case (f1: Function1[Any, Any], _) => f1(null)
        case (x, _) => x
      }
      rec(f, s)
    }
  }

  implicit class pimpedMap(s: String){
    def /(a: (String, Route)*) = s -> Node(a.toMap)
    def -(a: Nothing => Any) = s -> Leaf(a)
  }
}
