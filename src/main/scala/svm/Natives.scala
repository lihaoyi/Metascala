package svm

import annotation.tailrec
import java.util.concurrent.atomic.AtomicInteger
import model.Type

object Natives {

  var internedStrings: List[svm.Obj] = Nil
  def intern(x: svm.Obj) = {


    val interned = internedStrings.find(y => Virtualizer.fromVirtual(y) == Virtualizer.fromVirtual(x))
    interned match{
      case Some(i) => i
      case None =>
        internedStrings ::= x
        x
    }
  }
  def value(x: Any) = () => x
  def value1(x: Any) = (a: Any) => x
  def value2(x: Any) = (a: Any, b: Any) => x
  val noOp = value(())
  val noOp1 = value1(())
  val noOp2 = value2(())


  val properties = Map(
    "java.home" -> "C ->/java_home",
    "sun.boot.class.path" -> "C ->/classes",
    "file.encoding" ->"US_ASCII",
    "java.vendor" ->"Doppio",
    "java.version" -> "1.6",
    "java.vendor.url" -> "https ->//github.com/int3/doppio",
    "java.class.version" -> "50.0",
    "java.security.debug" -> "access,failure",
    "java.security.auth.debug" -> "access,failure",
    "java.specification.version" -> "1.6",
    "jdk.map.althashing.threshold" -> "-1",
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
  def trapped(implicit getClassFor: Type.Cls => Cls) = Route(
    "java"/(
      "lang"/(
        "ClassLoader"/(
          "getSystemClassLoader()Ljava/lang/ClassLoader;" - value(null)
        ),
        "System"/(
          "getProperty(L//String;)L//String;" - {
            (s: svm.Obj) =>
              Virtualizer.toVirtual[Any](properties(Virtualizer.fromVirtual[String](s)))

          },
          "getProperty(L//String;L//String;)L//String;" - {
            (s: svm.Obj, dflt: svm.Obj) =>
              properties.get(Virtualizer.fromVirtual[String](s))
                        .map(Virtualizer.toVirtual[svm.Obj])
                        .getOrElse(dflt)
          }
        )
      )
    ),
    "sun"/(
      "reflect"/(
        "Reflection"/(
          "registerMethodsToFilter(Ljava/lang/Class;[Ljava/lang/String;)V" - noOp2
        )
      ),
      "misc"/(
        "Hashing"/(
          "randomHashSeed(Ljava/lang/Object;)I" - value1(1)
        ),
        "Unsafe"/(
          "getUnsafe()Lsun/misc/Unsafe;" - value(svm.Obj("sun/misc/Unsafe"))
        ),
        "VM"/(
          "isBooted()Z" - value(true)
        )
      )

    )
  )
  def getObject(x: Any, i: Long) = {
    x match{
      case o: svm.Obj =>
        (for {
          m <- o.members
          k <- m.keys.find(_.hashCode == i)
        } yield m(k)).head

      case r: Array[Any] =>
        r(i.toInt)
    }
  }
  def putObject(x: Any, i: Long, b: Any) = {
    x match{
      case o: svm.Obj =>
        for {
          m <- o.members
          k <- m.keys.find(_.hashCode == i)
        } yield m(k) = b
      case r: Array[Any] =>
        r(i.toInt) = b
    }
  }

  def nativeX(thread: VmThread, stackTrace: () => List[StackTraceElement])(implicit getClassFor: Type.Cls => Cls): Route = Route(
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
        "reflect"/(
          "Array"/(
            "newArray(Ljava/lang/Class;I)Ljava/lang/Object;" - {
              (x: svm.ClsObj, n: Int) => new Array[svm.Obj](n)
            }
          )
        ),
        "Class"/(
          "registerNatives()V" - noOp,
          "getName0()L//String;" - ((s: svm.ClsObj) => Virtualizer.toVirtual[svm.Obj](s.name.replace("/", "."))),
          "forName0(L//String;)L//Class;" - ((s: svm.Obj) => Type.Cls(Virtualizer.fromVirtual[String](s)).obj),
          "forName0(L//String;ZL//ClassLoader;)L//Class;" - ((s: svm.Obj, x: Any, w: Any, y: Any) => Type.Cls(Virtualizer.fromVirtual[String](s)).obj),
          "getPrimitiveClass(L//String;)L//Class;"-((s: svm.Obj) => getClassFor(Type.Cls(Type.primitiveMap(Virtualizer.fromVirtual(s).asInstanceOf[String]))).obj),
          "getClassLoader0()L//ClassLoader;" - value1(null),
          "getDeclaringClass()L//Class;" - value(null),
          "getComponentType()Ljava/lang/Class;" - { (x: svm.ClsObj) =>
            x.name.splitAt(1) match {
              case ("[", rest) =>
                getClassFor(Type.Cls(Type.shortMap(rest))).obj
              case _ => null
            }
          },
          "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" - {
            (cls: svm.ClsObj, b: Int) => cls.getDeclaredConstructors().toArray
          },
          "getDeclaredFields0(Z)[L//reflect/Field;" - {(cls: svm.ClsObj, b: Int) =>
            cls.getDeclaredFields().toArray
          },
          "getDeclaredMethods0(Z)[L//reflect/Method;" - {(cls: svm.ClsObj, b: Int) =>
            cls.getDeclaredMethods().toArray
          },
          "getEnclosingMethod0()[L//Object;" - value(null),
          "getModifiers()I" - { (x: svm.ClsObj) => getClassFor(Type.Cls(x.name)).classData.access_flags },
          "getSuperclass()L//Class;" - {(x: svm.ClsObj) =>
            getClassFor(Type.Cls(x.name)).classData
                               .superType
                               .map(x => getClassFor(x).obj)
                               .getOrElse(null)
          },
          "isPrimitive()Z" - value1(false),
          "isInterface()Z" - ((x: svm.ClsObj) => (getClassFor(Type.Cls(x.name.replace(".", "/"))).classData.access_flags & Access.Interface) != 0),
          "isAssignableFrom(Ljava/lang/Class;)Z" - {(x: svm.ClsObj, y: svm.ClsObj) =>
            true
          },
          "isArray()Z" - ((_: svm.ClsObj).name.startsWith("[")),
          "desiredAssertionStatus0(L//Class;)Z" - value1(0)
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
            case (x: svm.Obj) => x
            case (a: Array[_]) => a.clone
          }},
          "registerNatives()V" - noOp,
          "getClass()L//Class;" - { (_: Any) match{
            case (x: svm.Obj) => x.cls.obj
            case a: Array[_] => Type.Cls(a.getClass.getName).obj
          }},
          "hashCode()I" - {(_: svm.Obj).hashCode()},
          "notify()V" - noOp1,
          "notifyAll()V" - noOp1
        ),
        "Runtime"/(
          "freeMemory()J" - value(1000000000L),
          "availableProcessors()I" - value(1)
        ),
        "String"/(
          "intern()L//String;" - intern _
        ),
        "System"/(
          "arraycopy(L//Object;IL//Object;II)V"-((src: Any, srcPos: Int, dest: Any, destPos: Int, length: Int) =>
            System.arraycopy(src, srcPos, dest, destPos, length)
          ),
          "currentTimeMillis()J" - value(System.currentTimeMillis()),
          "nanoTime()J" - value(System.nanoTime()),
          "initProperties(Ljava/util/Properties;)Ljava/util/Properties;" - value1(null),
          "identityHashCode(L//Object;)I"-((x: svm.Obj) => System.identityHashCode(x)),
          "registerNatives()V" - noOp,
          "setIn0(Ljava/io/InputStream;)V" - noOp1,
          "setOut0(Ljava/io/PrintStream;)V" - noOp1,
          "setErr0(Ljava/io/PrintStream;)V" - noOp1
        ),
        "Thread"/(
          "currentThread()L//Thread;" - value(thread.obj),
          "setPriority0(I)V" - noOp2,
          "start0()V" - noOp
        ),
        "Throwable"/(
          "fillInStackTrace(I)L//Throwable;" - { (throwable: svm.Obj, dummy: Int) =>
            throwable.members(0)("stackTrace") =
              stackTrace().map { f =>
              svm.Obj("java/lang/StackTraceElement",
                "declaringClass" -> Virtualizer.toVirtual(f.getClassName),
                "methodName" -> Virtualizer.toVirtual(f.getMethodName),
                "fileName" -> Virtualizer.toVirtual(f.getFileName),
                "lineNumber" -> Virtualizer.toVirtual(f.getLineNumber)
              )
            }.toArray
            throwable
          }
        ),
        "ref"/(
          "Reference$ReferenceHandler"/(
            "isAlive()Z" - value(false)
          )
        )
      ),
      "security"/(
        "AccessController"/(
          "doPrivileged(L//PrivilegedAction;)L/lang/Object;" - {
            (pa: svm.Obj) =>

              thread.prepInvoke(pa.cls, pa.cls.classData.methods.find(_.name == "run").get, Seq(pa))
          },
          "getStackAccessControlContext()L//AccessControlContext;" - value(svm.Obj("java/security/AccessControlContext")),
          "getInheritedAccessControlContext()L//AccessControlContext;" - value(svm.Obj("java/security/AccessControlContext"))
        )
      )
    ),
    "sun"/(
      "misc"/(
        "Unsafe"/(
          "arrayBaseOffset(Ljava/lang/Class;)I" - value1(0),
          "arrayIndexScale(Ljava/lang/Class;)I" - value1(1),
          "addressSize()I" - ((x: Any) => 4),
          "compareAndSwapInt(Ljava/lang/Object;JII)Z" - {(unsafe: Any, a: svm.Obj, i: Long, b: Int, c: Int) =>
            if (getObject(a, i) == b) {
              println("win")
              putObject(a, i, b)
              true
            }else{
              println("fail")
              false
            }
            true;
          },
          "putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V" - {
            (unsafe: svm.Obj, a: Any, i: Long, b: Any) => putObject(a, i, b)
          },
          "getObject(Ljava/lang/Object;J)Ljava/lang/Object;" - {
            (unsafe: svm.Obj, a: Any, i: Long) => getObject(a, i)
          },
          "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;" - {
            (unsafe: svm.Obj, a: Any, i: Long) => getObject(a, i)
          },
          "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z" - {
            (unsafe: svm.Obj, a: Any, i: Long, b: Any, c: Any) =>
              println("CAS" + a + "\t" + b + " for " + c + " found " + getObject(a, i))

              if (getObject(a, i) == b) {
                println("win")
                putObject(a, i, b)
                true
              }else{
                println("fail")
                false
              }
          },
          "objectFieldOffset(Ljava/lang/reflect/Field;)J" - {(unsafe: Any, x: svm.Obj) =>
            println(x)
            println(x.members)
            x(Type.Cls("java/lang/reflect/Field"), "slot").asInstanceOf[Int].toLong
          }
        ),
        "VM"/(
          "initialize()V" - noOp
        )
      ),

      "reflect"/(
        "NativeConstructorAccessorImpl"/(
          "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;" - {
            (constr: svm.Obj, args: Array[svm.Obj]) =>
              println("NewInstanceZero!")
              VM.log("NewInstanceZero!")
              val cls: svm.Cls = Type.Cls(constr.members(0)("clazz").asInstanceOf[svm.ClsObj].name)
              val newObj = new svm.Obj(cls)
              VM.log("|" + newObj)
              VM.log("|" + thread.threadStack.head.stack)
              thread.invoke(cls, cls.method("<init>", Type.Desc.read("()V")).get, Seq(newObj))
              VM.log("|" + thread.threadStack.head.stack)
              newObj
          }
        ),
        "Reflection"/(
          "getCallerClass(I)Ljava/lang/Class;" - { (n: Int) =>
            Type.Cls(stackTrace().drop(n).head.getClassName).obj
          },
          "getClassAccessFlags(Ljava/lang/Class;)I" - { (x: svm.ClsObj) =>
            getClassFor(Type.Cls(x.name)).classData.access_flags
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

          children.find(x =>

              x._1.replace("L///", "L" + parts(0) + "/" + parts(1) + "/" + parts(2) + "/")
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
