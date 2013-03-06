package svm

import annotation.tailrec
import java.util.concurrent.atomic.AtomicInteger
import model.Type

object Natives {


  def value(x: Any) = (vm: VmThread)  => x
  def value1(x: Any) = (vm: VmThread) => (a: Any) => x
  def value2(x: Any) = (vm: VmThread) => (a: Any, b: Any) => x
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

  val nativeX: NativeMap = {

    Map(
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
            "set(I)J" - {vt => (x: Int) => x.toLong}
            )
          ),
        "lang"/(
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;" - {
                vt =>(x: svm.ClsObj, n: Int) => new Array[svm.Obj](n)
              }
              )
            ),
          "Class"/(
            "registerNatives()V" - noOp,
            "getName0()L//String;" - {vt => (s: svm.TpeObj) =>
              import vt.vm
              Virtualizer.toVirtual[svm.Obj](s.tpe.unparse.replace("/", "."))
            },
            "forName0(L//String;)L//Class;" - {vt => (s: svm.Obj) =>
              import vt._
              Type.Cls(Virtualizer.fromVirtual[String](s)).obj
            },
            "forName0(L//String;ZL//ClassLoader;)L//Class;" - {vt => (s: svm.Obj, x: Any, w: Any, y: Any) =>
              import vt._
              Type.Cls(Virtualizer.fromVirtual[String](s)).obj
            },
            "getPrimitiveClass(L//String;)L//Class;" - {vt => (s: svm.Obj) =>
              import vt._
              Type.Cls(Type.primitiveMap(Virtualizer.fromVirtual(s).asInstanceOf[String])).obj
            },
            "getClassLoader0()L//ClassLoader;" - value1(null),
            "getDeclaringClass()L//Class;" - value(null),
            "getComponentType()Ljava/lang/Class;" - { vt => (x: svm.ClsObj) =>
              import vt._
              x.name.splitAt(1) match {
                case ("[", rest) =>
                  Type.Cls(Type.shortMap(rest)).obj
                case _ => null
              }
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" - {
              vt => (cls: svm.ClsObj, b: Int) => cls.getDeclaredConstructors().toArray
            },
            "getDeclaredFields0(Z)[L//reflect/Field;" - { vt => (cls: svm.TpeObj, b: Int) =>
              cls.getDeclaredFields()
            },
            "getDeclaredMethods0(Z)[L//reflect/Method;" - { vt => (cls: svm.TpeObj, b: Int) =>
              cls.getDeclaredMethods()
            },
            "getEnclosingMethod0()[L//Object;" - value(null),
            "getModifiers()I" - { vt => (x: svm.ClsObj) => import vt.vm._; Type.Cls(x.name).classData.access_flags },
            "getSuperclass()L//Class;" - { vt => (x: svm.ClsObj) =>
              import vt.vm
              import vm._
              Type.Cls(x.name).classData
                .superType
                .map(_.obj)
                .getOrElse(null)
            },
            "isPrimitive()Z" - value1(false),
            "isInterface()Z" - { vt => (x: svm.ClsObj) => import vt.vm._; (Type.Cls(x.name.replace(".", "/")).classData.access_flags & Access.Interface) != 0},
            "isAssignableFrom(Ljava/lang/Class;)Z" - { vt => (x: svm.ClsObj, y: svm.ClsObj) =>
              true
            },
            "isArray()Z" - ( vt => (_: svm.TpeObj).tpe.isInstanceOf[Type.Arr]),
            "desiredAssertionStatus0(L//Class;)Z" - value1(0)
          ),
          "ClassLoader"/(
            "getSystemClassLoader()Ljava/lang/ClassLoader;" - value(null)
          ),
          "Double"/(
            "doubleToRawLongBits(D)J"-{ vt => java.lang.Double.doubleToRawLongBits(_: Double)},
            "longBitsToDouble(J)D"-{ vt => java.lang.Double.longBitsToDouble(_: Long)}
          ),
          "Float"/(
            "intBitsToFloat(I)F"-{ vt => java.lang.Float.intBitsToFloat(_: Int)},
            "floatToRawIntBits(F)I"-{ vt => java.lang.Float.floatToIntBits(_: Float)}
          ),
          "Object"/(
            "clone()L//Object;" -  {(_: Any ) match{
              case (x: svm.Obj) => x
              case (a: Array[_]) => a.clone
            }},
            "registerNatives()V" - noOp,
            "getClass()L//Class;" - { vt => (_: Any) match{
              case (x: svm.Obj) => x.cls.obj
              case a: Array[_] => import vt.vm; Type.Arr.read(a.getClass.getName).obj
            }},
            "hashCode()I" - { vt => (_: svm.Obj).hashCode()},
            "notify()V" - noOp1,
            "notifyAll()V" - noOp1
            ),
          "Runtime"/(
            "freeMemory()J" - value(1000000000L),
            "availableProcessors()I" - value(1)
            ),
          "String"/(
            "intern()L//String;" - (vt => (x: Any) => x)
            ),
          "System"/(
            "arraycopy(L//Object;IL//Object;II)V"-( vt => (src: Any, srcPos: Int, dest: Any, destPos: Int, length: Int) =>
              System.arraycopy(src, srcPos, dest, destPos, length)
              ),
            "currentTimeMillis()J" - value(System.currentTimeMillis()),
            "getProperty(L//String;)L//String;" - {
              vt => (s: svm.Obj) =>
                import vt.vm
                Virtualizer.toVirtual[Any](properties(Virtualizer.fromVirtual[String](s)))

            },
            "getProperty(L//String;L//String;)L//String;" - {
              vt => (s: svm.Obj, dflt: svm.Obj) =>
                import vt.vm
                properties.get(Virtualizer.fromVirtual[String](s))
                  .map(Virtualizer.toVirtual[svm.Obj])
                  .getOrElse(dflt)
            },
            "nanoTime()J" - value(System.nanoTime()),
            "initProperties(Ljava/util/Properties;)Ljava/util/Properties;" - value1(null),
            "identityHashCode(L//Object;)I"-( vt => (x: svm.Obj) => System.identityHashCode(x)),
            "registerNatives()V" - noOp,
            "setIn0(Ljava/io/InputStream;)V" - noOp1,
            "setOut0(Ljava/io/PrintStream;)V" - noOp1,
            "setErr0(Ljava/io/PrintStream;)V" - noOp1
            ),
          "Thread"/(
            "currentThread()L//Thread;" - {vt => vt.obj},
            "setPriority0(I)V" - noOp2,
            "start0()V" - noOp
            ),
          "Throwable"/(
            "fillInStackTrace(I)L//Throwable;" - { vt => (throwable: svm.Obj, dummy: Int) =>
              import vt.vm;
              import vm._
              throwable.members(0)("stackTrace") =
                vt.getStackTrace.map { f =>
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
              vt => (pa: svm.Obj) =>

                vt.prepInvoke(pa.cls.classData.tpe, "run", Type.Desc.read("()V"), Seq(pa))
            },
            "getStackAccessControlContext()L//AccessControlContext;" - { vt => svm.Obj("java/security/AccessControlContext")(vt.vm)},
            "getInheritedAccessControlContext()L//AccessControlContext;" - { vt => svm.Obj("java/security/AccessControlContext")(vt.vm)}
            )
          )
        ),
      "sun"/(
        "misc"/(
          "Hashing"/(
            "randomHashSeed(Ljava/lang/Object;)I" - value1(1)
          ),
          "Unsafe"/(
            "arrayBaseOffset(Ljava/lang/Class;)I" - value1(0),
            "arrayIndexScale(Ljava/lang/Class;)I" - value1(1),
            "addressSize()I" - ((x: Any) => 4),
            "compareAndSwapInt(Ljava/lang/Object;JII)Z" - { vt => (unsafe: Any, a: svm.Obj, i: Long, b: Int, c: Int) =>
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
              vt => (unsafe: svm.Obj, a: Any, i: Long, b: Any) => putObject(a, i, b)
            },
            "getObject(Ljava/lang/Object;J)Ljava/lang/Object;" - {
              vt => (unsafe: svm.Obj, a: Any, i: Long) => getObject(a, i)
            },
            "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;" - {
              vt => (unsafe: svm.Obj, a: Any, i: Long) => getObject(a, i)
            },
            "getUnsafe()Lsun/misc/Unsafe;" - {vt =>
              import vt._
              svm.Obj("sun/misc/Unsafe")
            },
            "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z" - {
              vt => (unsafe: svm.Obj, a: Any, i: Long, b: Any, c: Any) =>
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
            "objectFieldOffset(Ljava/lang/reflect/Field;)J" - { vt => (unsafe: Any, x: svm.Obj) =>
              println(x)
              println(x.members)
              x(Type.Cls("java/lang/reflect/Field"), "slot").asInstanceOf[Int].toLong
            }
            ),
          "VM"/(
            "initialize()V" - noOp,
            "isBooted()Z" - value(true)
          )

          ),

        "reflect"/(
          "NativeConstructorAccessorImpl"/(
            "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;" - {
              vt => (constr: svm.Obj, args: Array[svm.Obj]) =>
                import vt.vm; import vm._
                val cls: svm.Cls = Type.Cls(constr.members(0)("clazz").asInstanceOf[svm.ClsObj].name)
                val newObj = new svm.Obj(cls)
                vt.invoke(cls.classData.tpe, "<init>", Type.Desc.read("()V"), Seq(newObj))
                newObj
            }
            ),
          "Reflection"/(
            "getCallerClass(I)Ljava/lang/Class;" - { vt => (n: Int) =>
              import vt.vm; import vm._
              Type.Cls(vt.getStackTrace.drop(n).head.getClassName).obj
            },
            "getClassAccessFlags(Ljava/lang/Class;)I" - { vt => (x: svm.ClsObj) =>
              import vt.vm._;
              Type.Cls(x.name).classData.access_flags
            },
            "registerMethodsToFilter(Ljava/lang/Class;[Ljava/lang/String;)V" - noOp2
          )
        )
      )
    ).toRoute()
  }

  type NativeMap = Map[(String, Type.Desc), VmThread => Seq[Any] => Any]

  implicit class pimpedRoute(m: Map[String, Any]){
    def toRoute(parts: List[String] = Nil): NativeMap = {
      m.flatMap{ case (k, v) =>
        v match{
          case thing: Map[String, Any] =>
            thing.toRoute(k :: parts).map {
              case ((path, desc), func) => ((k + "/" + path, desc), func)
            }

          case func: (VmThread => Any) =>
            val (name, descString) = k.splitAt(k.indexOf('('))
            val p = parts.reverse
            val fullDescString =
              descString.replace("L///", s"L${p(0)}/${p(1)}/${p(2)}/")
                        .replace("L//", s"L${p(0)}/${p(1)}/")
                        .replace("L/", s"L${p(0)}/")

            val desc = Type.Desc.read(fullDescString)

            val newFunc = (vt: VmThread) => (args: Seq[Any]) => func(vt) match{
              case f: ((Any, Any, Any, Any, Any) => Any) => f(args(0), args(1), args(2), args(3), args(4))
              case f: ((Any, Any, Any, Any) => Any) => f(args(0), args(1), args(2), args(3))
              case f: ((Any, Any, Any) => Any) => f(args(0), args(1), args(2))
              case f: ((Any, Any) => Any) => f(args(0), args(1))
              case f: (Any => Any) => f(args(0))
              case f: Any => f
            }
            Map((name, desc) -> newFunc)
        }
      }
    }
  }
  implicit class pimpedMap(s: String){
    def /(a: (String, Any)*) = s -> a.toMap
    def -(a: VmThread => Any) = s -> a
  }

}
