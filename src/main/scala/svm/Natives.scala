package svm

import annotation.tailrec
import java.util.concurrent.atomic.AtomicInteger

object Natives {

  var internedStrings: List[svm.Object] = Nil
  def intern(x: svm.Object) = {


    val interned = internedStrings.find(y => Virtualizer.fromVirtual(y) == Virtualizer.fromVirtual(x))
    interned match{
      case Some(i) => i
      case None =>
        internedStrings ::= x
        x
    }
  }
  val noOp = () => ()
  val noOp1 = (x: Any) => ()
  val noOp2 = (x: Any, y: Any) => ()
  val primitiveMap = Map(
    "boolean" -> "java/lang/Boolean",
    "byte" -> "java/lang/Byte",
    "short" -> "java/lang/Short",
    "int" -> "java/lang/Integer",
    "long" -> "java/lang/Long",
    "float" -> "java/lang/Float",
    "double" -> "java/lang/Double"
  )

  val properties = Map(
    "java.home" -> "C ->/java_home",
    "sun.boot.class.path" -> "C ->/classes",
    "file.encoding" ->"US_ASCII",
    "java.vendor" ->"Doppio",
    "java.version" -> "1.6",
    "java.vendor.url" -> "https ->//github.com/int3/doppio",
    "java.class.version" -> "50.0",
    "java.specification.version" -> "1.6",
    "line.separator" ->"\n",
    "file.separator" ->"/",
    "path.separator" ->":",
    "user.dir" -> ".",
    "user.home" ->".",
    "user.name" ->"DoppioUser",
    "os.name" ->"doppio",
    "os.arch" -> "js",
    "os.version" -> "0"

  )
  def trapped(implicit getClassFor: String => Class) = Route(
    "java"/(
      "lang"/(
        "System"/(
          "getProperty(L//String;)L//String;" - {
            (s: svm.Object) =>
              Virtualizer.toVirtual[svm.Object](properties(Virtualizer.fromVirtual[String](s)))

          },
          "getProperty(L//String;L//String;)L//String;" - {
            (s: svm.Object, dflt: svm.Object) =>
              properties.get(Virtualizer.fromVirtual[String](s))
                        .map(Virtualizer.toVirtual[svm.Object])
                        .getOrElse(dflt)
          }
        )
      )
    ),
    "sun"/(
      "reflect"/(
        "Reflection"/(
          "registerMethodsToFilter(Ljava/lang/Class;[Ljava/lang/String;)V" - (noOp2)
        )
      )
    )
  )


  def nativeX(thread: VmThread, stackTrace: () => List[StackTraceElement])(implicit getClassFor: String => Class): Route = Route(
    "java"/(
      "io"/(
        "FileInputStream"/(
          "initIDs()V" - noOp
        ),
        "FileOutputStream"/(
          "initIDs()V" - noOp
          ),
        "FileDescriptor"/(
          "initIDs()V" - noOp,
          "set(I)J" - {(x: Int) => x.toLong}
        )
      ),
      "lang"/(
        "Class"/(
          "registerNatives()V"-noOp,
          "getName0()L//String;" - ((s: svm.ClassObject) => Virtualizer.toVirtual[svm.Object](s.name.replace("/", "."))),
          "forName0(L//String;)L//Class;" - ((s: svm.Object) => Virtualizer.fromVirtual[String](s).obj),
          "forName0(L//String;ZL//ClassLoader;)L//Class;" - ((s: svm.Object, x: Any, w: Any, y: Any) => Virtualizer.fromVirtual[String](s).obj),
          "getPrimitiveClass(L//String;)L//Class;"-((s: svm.Object) => (new Object(getClassFor(primitiveMap(Virtualizer.fromVirtual(s).asInstanceOf[String]))))),
          "getClassLoader0()L//ClassLoader;"-((x: Any) => null),
          "getDeclaringClass()L//Class;" - {() =>
            null
          },
          "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" - {
            (cls: svm.ClassObject, b: Int) => cls.getDeclaredConstructors().toArray
          },
          "getDeclaredFields0(Z)[L//reflect/Field;" - {(cls: svm.ClassObject, b: Int) =>
            cls.getDeclaredFields().toArray
          },
          "getEnclosingMethod0()[L//Object;" - { () =>
            null
          },
          "isPrimitive()Z" - ((x: svm.ClassObject) => false),
          "isInterface()Z" - ((x: svm.ClassObject) => (getClassFor(x.name.replace(".", "/")).classData.access_flags & Access.Interface) != 0),
          "isAssignableFrom(Ljava/lang/Class;)Z" - {(x: svm.ClassObject, y: svm.ClassObject) =>
            true
          },
          "isArray()Z" - ((x: svm.ClassObject) => x.name.startsWith("[")),
          "desiredAssertionStatus0(L//Class;)Z"-((x: Any) => 0)
        ),
        "Double"/(
          "doubleToRawLongBits(D)J"-{ java.lang.Double.doubleToRawLongBits(_: Double)},
          "longBitsToDouble(J)D"-{ java.lang.Double.longBitsToDouble(_: Long)}
        ),
        "Float"/(
          "intBitsToFloat(I)F"-{ java.lang.Float.intBitsToFloat(_: Int)},
          "floatToRawIntBits(F)I"-{ java.lang.Float.floatToIntBits(_: Float)}
        ),
        "Object"/(
          "clone()L//Object;" -  {(_: Any ) match{
            case (x: svm.Object) => x
            case (a: Array[_]) => a.clone
          }},
          "registerNatives()V"-noOp,
          "getClass()L//Class;" - { (_: Any) match{
            case (x: svm.Object) => x.cls.obj
            case a: Array[_] => a.getClass.getName.obj
          }},
          "hashCode()I" - {(x: svm.Object) => x.hashCode()},
          "notify()V" - noOp1,
          "notifyAll()V" - noOp1
        ),
        "Runtime"/(
            "freeMemory()J" - {() => 1000000000L  }
        ),
        "String"/(
          "intern()L//String;" - intern _
        ),
        "System"/(
          "arraycopy(L//Object;IL//Object;II)V"-((src: Any, srcPos: Int, dest: Any, destPos: Int, length: Int) =>
            System.arraycopy(src, srcPos, dest, destPos, length)
          ),
          "currentTimeMillis()J"-(() => System.currentTimeMillis()),
          "nanoTime()J"-(() => System.nanoTime()),
          "initProperties(Ljava/util/Properties;)Ljava/util/Properties;" - {
            (x: svm.Object) => null
          },
          "identityHashCode(L//Object;)I"-((x: svm.Object) => System.identityHashCode(x)),
          "registerNatives()V"-noOp,
          "setIn0(Ljava/io/InputStream;)V" - noOp1,
          "setOut0(Ljava/io/PrintStream;)V" - noOp1,
          "setErr0(Ljava/io/PrintStream;)V" - noOp1
        ),
        "Thread"/(
          "currentThread()L//Thread;" - { () =>
            new svm.Object("java/lang/Thread",
              "name" -> "MyThread".toCharArray,
              "group" -> new svm.Object("java/lang/ThreadGroup"),
              "priority" -> 5
            )

          },
          "setPriority0(I)V" - { noOp2 },
          "start0()V" - { noOp }
        ),
        "Throwable"/(
          "fillInStackTrace(I)L//Throwable;" - { (throwable: svm.Object, dummy: Int) =>
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
        ),
        "ref"/(
          "Reference$ReferenceHandler"/(
            "isAlive()Z" - {() => false}
          )
        )
      ),
      "security"/(
        "AccessController"/(
          "doPrivileged(L//PrivilegedAction;)L/lang/Object;" - {
            (pa: svm.Object) =>

              thread.prepInvoke(pa.cls, pa.cls.classData.methods.find(_.name == "run").get, Seq(pa))
          },
          "getStackAccessControlContext()L//AccessControlContext;" - {
            () => new svm.Object("java/security/AccessControlContext")
          },
          "getInheritedAccessControlContext()L//AccessControlContext;" - {
            () => new svm.Object("java/security/AccessControlContext")
          }
        )
      )
    ),
    "sun"/(
      "misc"/(
        "Unsafe"/(
          "arrayBaseOffset(Ljava/lang/Class;)I" - ((x: Any) => 0),
          "arrayIndexScale(Ljava/lang/Class;)I" - ((x: Any) => 1),
          "addressSize()I" - ((x: Any) => 4),
          "compareAndSwapInt(Ljava/lang/Object;JII)Z" - {(s: Any, x: svm.Object, offset: Int, expected: Int, target: Int) =>
            val key = x.members.head.keys.toSeq(offset.toInt)

            if (x(x.cls.name, key) == expected) x(x.cls.name, key) = target
            true;
          },
          "objectFieldOffset(Ljava/lang/reflect/Field;)J" - {(x: svm.Object) => 0 }
        ),
        "VM"/(
          "initialize()V" - noOp
        )
      ),

      "reflect"/(
        "Reflection"/(
          "getCallerClass(I)Ljava/lang/Class;" - { (n: Int) =>
            stackTrace().drop(n).head.getClassName.obj
          },
          "getClassAccessFlags(Ljava/lang/Class;)I" - { (x: svm.ClassObject) =>
            getClassFor(x.name).classData.access_flags
          }
        )
      )
    )
  )

  object Route{
    def apply(a: (String, Route)*) = Node(a.toMap)
  }
  trait Route{
    def lookup(s: String, parts: List[String] = Nil): Option[Leaf]
  }
  case class Node(children: Map[String, Route]) extends Route{
    def lookup(s: String, parts: List[String] = Nil) = {

        if (s.indexOf('/') != -1 && s.indexOf('/') < s.indexOf('(')){
          val Array(first, rest) =s.split("/", 2)
          children.get(first).flatMap(_.lookup(rest, parts :+ first))
        }else{

          children.find(_._1.replace("L///", "L" + parts(0) + "/" + parts(1) + "/" + parts(2) + "/")
              .replace("L//", "L" + parts(0) + "/" + parts(1) + "/")
              .replace("L/", "L" + parts(0) + "/") == s
          ).flatMap(_._2.lookup("", Nil))
        }

    }
  }

  implicit def func0(f: () => Any) = (x: Any) => f()
  implicit def func2(f: (Nothing, Nothing) => Any) = f.curried
  implicit def func3(f: (Nothing, Nothing, Nothing) => Any) = f.curried
  implicit def func4(f: (Nothing, Nothing, Nothing, Nothing) => Any) = f.curried
  implicit def func5(f: (Nothing, Nothing, Nothing, Nothing, Nothing) => Any) = f.curried
  case class Leaf(f: Nothing => Any) extends Route{
    def lookup(s: String, parts: List[String] = Nil) = {

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
