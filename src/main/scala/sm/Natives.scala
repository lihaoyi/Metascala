package sm

import imm.Type.Prim.Info
import imm.{Access, Type}
import rt.Cls
import vrt.Obj
import java.io.{DataInputStream}


object Natives{
  val default = new DefaultNatives {}
  type NativeMap = Map[(String, Type.Desc), VmThread => Seq[vrt.Val] => vrt.Val]
  type NativeSeq = Seq[((String, Type.Desc), VmThread => Seq[vrt.Val] => vrt.Val)]
}
trait Natives{
  val trapped: Natives.NativeMap
  val trappedIndex: Natives.NativeSeq
  val fileLoader: String => Option[Array[Byte]]
}
object NativeUtils{
  def value(x: => vrt.Val) = (vm: VmThread) => x
  def value1(x: => vrt.Val) = (vm: VmThread) => (a: vrt.Val) => x
  def value2(x: => vrt.Val) = (vm: VmThread) => (a: vrt.Val, b: vrt.Val) => x
  val noOp = value(vrt.Unit)
  val noOp1 = value1(vrt.Unit)
  val noOp2 = value2(vrt.Unit)

  implicit class pimpedRoute(val m: Seq[(String, Any)]) extends AnyVal{
    def toRoute(parts: List[String] = Nil): Natives.NativeSeq = {
      m.flatMap{ case (k, v) =>
        v match{
          case thing: Seq[(String, Any)] =>
            thing.toRoute(k :: parts).map {
              case ((path, desc), func) => ((k + "/" + path, desc), func)
            }

          case func: (VmThread => Any) =>
            val (name, descString) = k.splitAt(k.indexOf('('))
            val p = parts.reverse

            val fullDescString =
              descString
                .replaceAllLiterally("L//", s"L${p(0)}/${p(1)}/")
                .replaceAllLiterally("L/", s"L${p(0)}/")

            val desc = Type.Desc.read(fullDescString)

            type V = vrt.Val
            val newFunc = (vt: VmThread) => (args: Seq[vrt.Val]) => func(vt) match{
              case f: ((V, V, V, V, V) => V) => f(args(0), args(1), args(2), args(3), args(4))
              case f: ((V, V, V, V) => V) => f(args(0), args(1), args(2), args(3))
              case f: ((V, V, V) => V) => f(args(0), args(1), args(2))
              case f: ((V, V) => V) => f(args(0), args(1))
              case f: (V => V) => f(args(0))
              case f: V => f
            }
            Vector((name, desc) -> newFunc)
        }
      }
    }
  }
  implicit class pimpedMap(val s: String) extends AnyVal{
    def /(a: (String, Any)*) = s -> a
    def x(a: VmThread => vrt.Val) = s -> a
    def x1(a: VmThread => Nothing => vrt.Val) = s -> a
    def x2(a: VmThread => (Nothing, Nothing) => vrt.Val) = s -> a
    def x3(a: VmThread => (Nothing, Nothing, Nothing) => vrt.Val) = s -> a
    def x4(a: VmThread => (Nothing, Nothing, Nothing, Nothing) => vrt.Val) = s -> a
    def x5(a: VmThread => (Nothing, Nothing, Nothing, Nothing, Nothing) => vrt.Val) = s -> a
    def x6(a: VmThread => (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) => vrt.Val) = s -> a
  }
}
trait DefaultNatives extends Natives{

  import NativeUtils._
  val fileLoader = (name: String) => {
    val slashName = s"/$name"

    val loaded = getClass.getResourceAsStream(slashName)
    if (loaded == null) None
    else {
      val stream = new DataInputStream(loaded)
      val bytes = new Array[Byte](stream.available())
      stream.readFully(bytes)
      //imm.Util.printClass(bytes)
      Some(bytes)
    }
  }

  val properties = Map[String, String](
    /*"java.home" -> "C ->/java_home",
    "sun.boot.class.path" -> "C ->/classes",
    "file.encoding" ->"US_ASCII",
    "java.ext.dirs" -> "",
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
    "os.version" -> "0"*/

  )

  def getObject(x: vrt.Val, i: Long): vrt.Val = {

    x match{
      case o: vrt.Obj =>
        val matches = for {
          (f, m) <- o.members
          if m.hashCode == i
        } yield m

      case r: vrt.Arr =>
        r.backing(i.toInt).asInstanceOf[vrt.Val]
    }
  }
  def putObject(x: vrt.Val, i: Long, b: vrt.Val): Unit = {
    x match{
      case o: vrt.Obj =>
        for {
          (f, m) <- o.members
          if m.hashCode == i
        } yield m() = b
      case r: vrt.Arr.Obj =>
        r.backing(i.toInt) = b.asInstanceOf[vrt.Val]
    }
  }

  val trappedIndex = {
    Seq(
      "java"/(
        "io"/(
          "FileInputStream"/(
            "initIDs()V" x noOp
            ),
          "FileOutputStream"/(
            "initIDs()V" x noOp
            ),
          "FileDescriptor"/(
            "initIDs()V" x noOp,
            "set(I)J" x1 {vt => (x: vrt.Int) => x.toLong: vrt.Val}
            )
          ),
        "lang"/(
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;" x2 {
                vt =>(x: vrt.Cls, n: vrt.Int) => new Array[Obj](n): vrt.Val
              }
            )
          ),
          "Class"/(
            "registerNatives()V" x noOp,
            "getName0()L//String;" x1 {vt => (s: vrt.Type) =>
              import vt.vm
              s.tpe.unparse.replace("/", "."): vrt.Val
            },
            "forName0(L//String;)L//Class;" x1 {vt => (s: vrt.Obj) =>
              import vt._
              Type.Cls(s).obj
            },
            "forName0(L//String;ZL//ClassLoader;)L//Class;" x3 {vt => (s: vrt.Obj, w: Any, y: Any) =>
              import vt._
              Type.Cls(s.replace('.', '/')).obj
            },
            "getPrimitiveClass(L//String;)L//Class;" x1 {vt => (s: vrt.Obj) =>
              import vt._
              Type.Cls(imm.Type.Prim.Info.all.find(_.name == (s: String)).get.boxName).obj
            },
            "getClassLoader0()L//ClassLoader;" x1 value1(vrt.Null),
            "getDeclaringClass()L//Class;" x value(vrt.Null),
            "getComponentType()Ljava/lang/Class;" x1 { vt => (x: vrt.Type) =>
              import vt._
              x.tpe match{
                case imm.Type.Arr(inner) => inner.obj
                case _ => vrt.Null
              }
            },
            "getInterfaces()[Ljava/lang/Class;" x1 {
              vt => (cls: vrt.Cls) => cls.getInterfaces()
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" x2 {
              vt => (cls: vrt.Cls, b: Int) => cls.getDeclaredConstructors()
            },
            "getDeclaredFields0(Z)[L//reflect/Field;" x2 { vt => (cls: vrt.Type, b: vrt.Int) =>
              cls.getDeclaredFields()
            },
            "getDeclaredMethods0(Z)[L//reflect/Method;" x2 { vt => (cls: vrt.Type, b: vrt.Int) =>
              cls.getDeclaredMethods()
            },
            "getEnclosingMethod0()[L//Object;" x value(vrt.Null),
            "getModifiers()I" x1 { vt => (x: vrt.Cls) => import vt.vm._; Type.Cls(x.name).clsData.access_flags },
            "getSuperclass()L//Class;" x1 { vt => (x: vrt.Cls) =>
              import vt.vm
              import vm._
              Type.Cls(x.name).clsData
                .superType
                .map(_.obj)
                .getOrElse(vrt.Null)
            },
            "getRawAnnotations()[B" x1 value1(new vrt.Arr.Prim(new Array[Boolean](0))),
            "isPrimitive()Z" x1 { vt => (x: vrt.Type) => x.tpe.isInstanceOf[Type.Prim]
            },
            "isInterface()Z" x1 { vt => (x: vrt.Cls) => import vt.vm._; ((Type.Cls(x.name.replace(".", "/")).clsData.access_flags & Access.Interface) != 0): vrt.Val},
            "isAssignableFrom(L//Class;)Z" x2 { vt => (x: vrt.Type, y: vrt.Type) => true: vrt.Val},
            "isArray()Z" x1 { vt => (x: vrt.Type) => x.tpe.isInstanceOf[Type.Arr]
            },
            "desiredAssertionStatus0(L//Class;)Z" x1 value1(0: vrt.Val)
          ),
          "ClassLoader"/(
            "getSystemClassLoader()L//ClassLoader;" x value(vrt.Null),
            "getCaller(I)L//Class;" x1 { vt => (x: Int) =>
              vt.threadStack(x).runningClass.obj
            },
            "getResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;" x2 { vt => (cl: vrt.Obj, s: vrt.Obj) =>
              import vt.vm
              val str: String = s
              if (!str.endsWith(".properties"))
                fileLoader(str) match{
                  case None => vrt.Null
                  case Some(bytes) =>
                    vrt.Obj("java.io.ByteArrayInputStream",
                      "buf" -> new vrt.Arr.Prim(bytes),
                      "pos" -> 0,
                      "mark" -> 0,
                      "count" -> bytes.length
                    )
                }
              else vrt.Null
            },
            "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;" x1 { vt => (s: vrt.Obj) =>

              import vt.vm
              val str: String = s
              if (!str.endsWith(".properties"))
                fileLoader(str) match{
                  case None => vrt.Null
                  case Some(bytes) =>
                    vrt.Obj("java/io/ByteArrayInputStream",
                      "buf" -> new vrt.Arr.Prim(bytes),
                      "pos" -> 0,
                      "mark" -> 0,
                      "count" -> bytes.length
                    )
                }
              else vrt.Null
            }

          ),
          "Double"/(
            "doubleToRawLongBits(D)J" x1 { vt => (x: vrt.Double) => java.lang.Double.doubleToRawLongBits(x): vrt.Val},
            "longBitsToDouble(J)D" x1 { vt => (x: vrt.Long) => java.lang.Double.longBitsToDouble(x): vrt.Val}
          ),
          "Float"/(
            "intBitsToFloat(I)F" x1 { vt => (x: vrt.Int) => java.lang.Float.intBitsToFloat(x): vrt.Val},
            "floatToRawIntBits(F)I" x1 { vt => (x: vrt.Float) => java.lang.Float.floatToIntBits(x): vrt.Val}
          ),
          "Object"/(
            "clone()L//Object;" x1 {vt => (_: Any ) match{
              case (x: vrt.Obj) => x
              case (x: vrt.Arr) => x
            }},
            "registerNatives()V" x noOp,
            "getClass()L//Class;" x1 { vt => (x: vrt.Val) =>

              x match{
              case (x: vrt.Obj) => x.cls.obj
              case (x: vrt.Arr) => imm.Type.Arr(x.tpe).obj(vt.vm)
            }},
            "hashCode()I" x1 { vt => (_: Obj).hashCode()},
            "notify()V" x1 noOp1,
            "notifyAll()V" x1 noOp1
            ),
          "Runtime"/(
            "freeMemory()J" x value(1000000000L: vrt.Val),
            "availableProcessors()I" x value(1: vrt.Val)
            ),
          "String"/(
            "intern()L//String;" x1 (vt => (x: vrt.Obj) => vt.vm.InternedStrings(x))
            ),
          "System"/(
            "arraycopy(L//Object;IL//Object;II)V" x5 { vt => (src: vrt.Arr, srcPos: vrt.Int, dest: vrt.Arr, destPos: vrt.Int, length: vrt.Int) =>

              System.arraycopy(src.backing, srcPos, dest.backing, destPos, length)
            },
            "currentTimeMillis()J" x value(System.currentTimeMillis()),
            "getProperty(L//String;)L//String;" x1 {
              vt => (s: Obj) =>
                import vt.vm
                properties.get(s)
                          .map(x => x: vrt.Val)
                          .getOrElse(vrt.Null)

            },
            "getProperty(L//String;L//String;)L//String;" x2 {
              vt => (s: Obj, dflt: Obj) =>
                import vt.vm
                properties.get(s)
                          .map(x => x: vrt.Val)
                          .getOrElse(dflt)
            },
            "nanoTime()J" x value(System.nanoTime()),
            "initProperties(Ljava/util/Properties;)Ljava/util/Properties;" x1 value1(vrt.Null),
            "identityHashCode(L//Object;)I" x1 ( vt => (x: Obj) => System.identityHashCode(x): vrt.Val),
            "registerNatives()V" x noOp,
            "setIn0(Ljava/io/InputStream;)V" x1 noOp1,
            "setOut0(Ljava/io/PrintStream;)V" x1 noOp1,
            "setErr0(Ljava/io/PrintStream;)V" x1 noOp1
            ),
          "Thread"/(
            "currentThread()L//Thread;" x {vt => vt.obj},
            "isAlive()Z" x value(false: vrt.Val),
            "setPriority0(I)V" x2 noOp2,
            "start0()V" x noOp
            ),
          "Throwable"/(
            "fillInStackTrace(I)L//Throwable;" x2 { vt => (throwable: Obj, dummy: vrt.Int) =>
              import vt.vm;
              import vm._
              throwable(throwable.refType, "stackTrace") =
                new vrt.Arr.Obj(
                  imm.Type.Cls("java/lang/StackTraceElement"),
                  vt.getStackTrace.map { f =>
                    vrt.Obj("java/lang/StackTraceElement",
                      "declaringClass" -> f.getClassName,
                      "methodName" -> f.getMethodName,
                      "fileName" -> f.getFileName,
                      "lineNumber" -> f.getLineNumber
                    )
                  }.toArray
                )
              throwable
            }
          ),
          "ref"/(
            "Reference$ReferenceHandler"/(
              "isAlive()Z" x value(false: vrt.Val)
              )
            )
          ),
        "security"/(
          "AccessController"/(
            "doPrivileged(L//PrivilegedAction;)L/lang/Object;" x1 {
              vt => (pa: Obj) =>
                vt.prepInvoke(pa.cls.clsData.tpe, "run", pa.cls.clsData.methods.find(_.name == "run").get.desc, Seq(pa))
            },
            "getStackAccessControlContext()L//AccessControlContext;" x { vt => vrt.Obj("java/security/AccessControlContext")(vt.vm)},
            "getAccessControlContext()L//AccessControlContext;" x { vt => vrt.Obj("java/security/AccessControlContext")(vt.vm)},
            "getInheritedAccessControlContext()L//AccessControlContext;" x { vt => vrt.Obj("java/security/AccessControlContext")(vt.vm)}
            )
          )
        ),
      "scala"/(
        "Predef$"/(
          "println(Ljava/lang/String;)V" x2 {
            vt => (x: vrt.Obj, y: vrt.Obj) =>
              println("VIRTUAL " + vrt.unvirtString(y))
          },
          "println(Ljava/lang/Object;)V" x2 {
            vt => (x: vrt.Obj, y: vrt.Obj) =>
              println("VIRTUAL " + vrt.unvirtString(y))
          }

        )
      ),
      "sun"/(
        "misc"/(
          "Hashing"/(
            "randomHashSeed(Ljava/lang/Object;)I" x1 value1(1)
          ),
          "Unsafe"/(
            "arrayBaseOffset(Ljava/lang/Class;)I" x1 value1(0),
            "arrayIndexScale(Ljava/lang/Class;)I" x1 value1(1),
            "addressSize()I" x1 value1(4),
            "compareAndSwapInt(Ljava/lang/Object;JII)Z" x5 { vt => (unsafe: Any, a: vrt.Obj, i: vrt.Long, b: vrt.Int, c: vrt.Int) =>
              if (getObject(a, i) == b) {
                //putObject(a, i, b)
                true
              }else{
                false
              }
              true: vrt.Val
            },
            "ensureClassInitialized(Ljava/lang/Class;)V" x2 noOp2,
            "putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V" x4 {
              vt => (unsafe: vrt.Obj, a: vrt.Val, i: vrt.Long, b: vrt.Val) => putObject(a, i, b)
            },
            "getObject(Ljava/lang/Object;J)Ljava/lang/Object;" x3 {
              vt => (unsafe: vrt.Obj, a: vrt.Val, i: vrt.Long) => getObject(a, i)
            },
            "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;" x3 {
              vt => (unsafe: vrt.Obj, a: vrt.Val, i: vrt.Long) => getObject(a, i)
            },
            "getUnsafe()Lsun/misc/Unsafe;" x {vt =>
              import vt._
              vm.theUnsafe
            },
            "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z" x5 {
              vt => (unsafe: Obj, a: vrt.Val, i: vrt.Long, b: vrt.Val, c: vrt.Val) =>
                if (getObject(a, i) == b) {
                  putObject(a, i, b)
                  true
                }else{
                  false
                }
            },
            "objectFieldOffset(Ljava/lang/reflect/Field;)J" x2 { vt => (unsafe: Any, x: Obj) =>
              x(Type.Cls("java/lang/reflect/Field"), "slot").asInstanceOf[vrt.Int].toLong
            },
            "staticFieldOffset(Ljava/lang/reflect/Field;)J" x2 { vt => (unsafe: Any, x: Obj) =>
              x(Type.Cls("java/lang/reflect/Field"), "slot").asInstanceOf[vrt.Int].toLong
            },
            "staticFieldBase(Ljava/lang/reflect/Field;)Ljava/lang/Object;" x2 { vt => (unsafe: Any, x: Obj) =>
              import vt._
              vm.theUnsafe
            }
          ),
          "VM"/(
            "initialize()V" x noOp,
            "isBooted()Z" x value(true),
            "getSavedProperty(Ljava/lang/String;)Ljava/lang/String;" x1 {
              vt => (s: Obj) =>
                import vt.vm
                properties.get(s)
                          .map(x => x: vrt.Val)
                          .getOrElse(vrt.Null)
            }
          )

        ),

        "reflect"/(
          "NativeConstructorAccessorImpl"/(
            "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;" x2 {
              vt => (constr: Obj, args: Array[Obj]) =>
                import vt.vm; import vm._
                val cls: Cls = imm.Type.Cls(constr.members.find(_._1.name == "clazz").get._2.asInstanceOf[vrt.Cls].name)
                val newObj = new Obj(cls)
                vt.invoke(cls.clsData.tpe, "<init>", Type.Desc.read("()V"), Seq(newObj))
                newObj
            }
            ),
          "Reflection"/(
            "getCallerClass(I)Ljava/lang/Class;" x1 { vt => (n: vrt.Int) =>
              import vt.vm; import vm._
              Type.Cls(vt.getStackTrace.drop(n).head.getClassName).obj
            },
            "getClassAccessFlags(Ljava/lang/Class;)I" x1 { vt => (x: vrt.Cls) =>
              import vt.vm._;
              Type.Cls(x.name).clsData.access_flags
            },
            "registerMethodsToFilter(Ljava/lang/Class;[Ljava/lang/String;)V" x2 noOp2
          )
        )
      )
    ).toRoute()
  }

  val trapped = trappedIndex.toMap




}

