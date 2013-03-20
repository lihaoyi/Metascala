package sm

import imm.{Access, Type}
import virt.Obj
import java.io.{DataInputStream}


object Natives{
  val default = new DefaultNatives {}
  type NativeMap = Map[(String, Type.Desc), VmThread => Seq[virt.Val] => virt.Val]
  type NativeSeq = Seq[((String, Type.Desc), VmThread => Seq[virt.Val] => virt.Val)]
}
trait Natives{
  val properties: Map[String, String]
  val trapped: Natives.NativeMap
  val fileLoader: String => Option[Array[Byte]]
}
object NativeUtils{
  def value(x: virt.Val) = (vm: VmThread) => x
  def value1(x: virt.Val) = (vm: VmThread) => (a: virt.Val) => x
  def value2(x: virt.Val) = (vm: VmThread) => (a: virt.Val, b: virt.Val) => x
  val noOp = value(virt.Unit)
  val noOp1 = value1(virt.Unit)
  val noOp2 = value2(virt.Unit)

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

            type V = virt.Val
            val newFunc = (vt: VmThread) => (args: Seq[virt.Val]) => func(vt) match{
              case f: ((V, V, V, V, V) => V) => f(args(0), args(1), args(2), args(3), args(4))
              case f: ((V, V, V, V) => V) => f(args(0), args(1), args(2), args(3))
              case f: ((V, V, V) => V) => f(args(0), args(1), args(2))
              case f: ((V, V) => V) => f(args(0), args(1))
              case f: (V => V) => f(args(0))
              case f: V => f
            }
            Seq((name, desc) -> newFunc)
        }
      }
    }
  }
  implicit class pimpedMap(val s: String) extends AnyVal{
    def /(a: (String, Any)*) = s -> a
    def x(a: VmThread => virt.Val) = s -> a
    def x1(a: VmThread => Nothing => virt.Val) = s -> a
    def x2(a: VmThread => (Nothing, Nothing) => virt.Val) = s -> a
    def x3(a: VmThread => (Nothing, Nothing, Nothing) => virt.Val) = s -> a
    def x4(a: VmThread => (Nothing, Nothing, Nothing, Nothing) => virt.Val) = s -> a
    def x5(a: VmThread => (Nothing, Nothing, Nothing, Nothing, Nothing) => virt.Val) = s -> a
    def x6(a: VmThread => (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) => virt.Val) = s -> a
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

  def getObject(x: virt.Val, i: Long): virt.Val = {
    x match{
      case o: virt.Obj =>
        (for {
          m <- o.members
          k <- m.keys.find(_.hashCode == i)
        } yield m(k)).head

      case r: virt.Arr =>
        r.backing(i.toInt).asInstanceOf[virt.Val]
    }
  }
  def putObject(x: virt.Val, i: Long, b: virt.Val): Unit = {
    x match{
      case o: virt.Obj =>
        for {
          m <- o.members
          k <- m.keys.find(_.hashCode == i)
        } yield m(k) = b
      case r: virt.ObjArr =>
        r.backing(i.toInt) = b.asInstanceOf[virt.Val]
    }
  }

  val trapped: Natives.NativeMap = {
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
            "set(I)J" x1 {vt => (x: virt.Int) => x.toLong: virt.Val}
            )
          ),
        "lang"/(
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;" x2 {
                vt =>(x: virt.Cls, n: Int) => new Array[Obj](n): virt.Val
              }
            )
          ),
          "Class"/(
            "registerNatives()V" x noOp,
            "getName0()L//String;" x1 {vt => (s: virt.Type) =>
              import vt.vm
              s.tpe.unparse.replace("/", "."): virt.Val
            },
            "forName0(L//String;)L//Class;" x1 {vt => (s: virt.Obj) =>
              import vt._
              Type.Cls(s).obj
            },
            "forName0(L//String;ZL//ClassLoader;)L//Class;" x3 {vt => (s: virt.Obj, w: Any, y: Any) =>
              import vt._
              Type.Cls(s).obj
            },
            "getPrimitiveClass(L//String;)L//Class;" x1 {vt => (s: virt.Obj) =>
              import vt._
              Type.Cls(Type.primitiveMap(s)).obj
            },
            "getClassLoader0()L//ClassLoader;" x1 value1(virt.Null),
            "getDeclaringClass()L//Class;" x value(virt.Null),
            "getComponentType()Ljava/lang/Class;" x1 { vt => (x: virt.Type) =>
              import vt._
              x.tpe match{
                case imm.Type.Arr(inner) => inner.obj
                case _ => virt.Null
              }
            },
            "getInterfaces()[Ljava/lang/Class;" x1 {
              vt => (cls: virt.Cls) => cls.getInterfaces()
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" x2 {
              vt => (cls: virt.Cls, b: Int) => cls.getDeclaredConstructors()
            },
            "getDeclaredFields0(Z)[L//reflect/Field;" x2 { vt => (cls: virt.Type, b: virt.Int) =>
              virt.virtualize(cls.getDeclaredFields())(vt.vm)
            },
            "getDeclaredMethods0(Z)[L//reflect/Method;" x2 { vt => (cls: virt.Type, b: virt.Int) =>
              virt.virtualize(cls.getDeclaredMethods())(vt.vm)
            },
            "getEnclosingMethod0()[L//Object;" x value(virt.Null),
            "getModifiers()I" x1 { vt => (x: virt.Cls) => import vt.vm._; Type.Cls(x.name).clsData.access_flags },
            "getSuperclass()L//Class;" x1 { vt => (x: virt.Cls) =>
              import vt.vm
              import vm._
              Type.Cls(x.name).clsData
                .superType
                .map(_.obj)
                .getOrElse(virt.Null)
            },
            "getRawAnnotations()[B" x1 value1(new virt.PrimArr(Type.Prim('B'), new Array[Boolean](0))),
            "isPrimitive()Z" x1 { vt => (x: virt.Type) =>
              println("isPrimitive!!")
              println(x.tpe)
              x.tpe.isInstanceOf[Type.Prim]
            },
            "isInterface()Z" x1 { vt => (x: virt.Cls) => import vt.vm._; ((Type.Cls(x.name.replace(".", "/")).clsData.access_flags & Access.Interface) != 0): virt.Val},
            "isAssignableFrom(L//Class;)Z" x2 { vt => (x: virt.Type, y: virt.Type) => true: virt.Val},
            "isArray()Z" x1 { vt => (x: virt.Type) =>
              println("isArray!!")
              println(x.tpe)
              println(x.tpe.isInstanceOf[Type.Arr])
              x.tpe.isInstanceOf[Type.Arr]
            },
            "desiredAssertionStatus0(L//Class;)Z" x1 value1(0: virt.Val)
          ),
          "ClassLoader"/(
            "getSystemClassLoader()L//ClassLoader;" x value(virt.Null),
            "getCaller(I)L//Class;" x1 { vt => (x: Int) =>
              vt.threadStack(x).runningClass.obj
            },
            "getResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;" x2 { vt => (cl: virt.Obj, s: virt.Obj) =>
              import vt.vm
              val str: String = s

              fileLoader(str) match{
                case None => virt.Null
                case Some(bytes) =>
                  virt.Obj("java.io.ByteArrayInputStream",
                    "buf" -> bytes,
                    "pos" -> 0,
                    "mark" -> 0,
                    "count" -> bytes.length
                  )
              }
            },
            "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;" x1 { vt => (s: virt.Obj) =>

              import vt.vm
              val str: String = s

              fileLoader(str) match{
                case None => virt.Null
                case Some(bytes) =>
                  virt.Obj("java.io.ByteArrayInputStream",
                    "buf" -> bytes,
                    "pos" -> 0,
                    "mark" -> 0,
                    "count" -> bytes.length
                  )
              }
            }

          ),
          "Double"/(
            "doubleToRawLongBits(D)J" x1 { vt => (x: virt.Double) => java.lang.Double.doubleToRawLongBits(x): virt.Val},
            "longBitsToDouble(J)D" x1 { vt => (x: virt.Long) => java.lang.Double.longBitsToDouble(x): virt.Val}
          ),
          "Float"/(
            "intBitsToFloat(I)F" x1 { vt => (x: virt.Int) => java.lang.Float.intBitsToFloat(x): virt.Val},
            "floatToRawIntBits(F)I" x1 { vt => (x: virt.Float) => java.lang.Float.floatToIntBits(x): virt.Val}
          ),
          "Object"/(
            "clone()L//Object;" x1 {vt => (_: Any ) match{
              case (x: virt.Obj) => x
            }},
            "registerNatives()V" x noOp,
            "getClass()L//Class;" x1 { vt => (x: virt.Val) =>
              println("GETTING CLASS " + x)
              x match{
              case (x: virt.Obj) => x.cls.obj
              case (x: virt.Arr) => imm.Type.Arr(x.tpe).obj(vt.vm)
            }},
            "hashCode()I" x1 { vt => (_: Obj).hashCode()},
            "notify()V" x1 noOp1,
            "notifyAll()V" x1 noOp1
            ),
          "Runtime"/(
            "freeMemory()J" x value(1000000000L: virt.Val),
            "availableProcessors()I" x value(1: virt.Val)
            ),
          "String"/(
            "intern()L//String;" x1 (vt => (x: virt.Obj) => vt.vm.InternedStrings(x))
            ),
          "System"/(
            "arraycopy(L//Object;IL//Object;II)V" x5 { vt => (src: virt.Arr, srcPos: virt.Int, dest: virt.Arr, destPos: virt.Int, length: virt.Int) =>

              System.arraycopy(src.backing, srcPos, dest.backing, destPos, length)
            },
            "currentTimeMillis()J" x value(System.currentTimeMillis()),
            "getProperty(L//String;)L//String;" x1 {
              vt => (s: Obj) =>
                import vt.vm
                properties.get(s)
                          .map(x => x: virt.Val)
                          .getOrElse(virt.Null)

            },
            "getProperty(L//String;L//String;)L//String;" x2 {
              vt => (s: Obj, dflt: Obj) =>
                import vt.vm
                properties.get(s)
                          .map(x => x: virt.Val)
                          .getOrElse(dflt)
            },
            "nanoTime()J" x value(System.nanoTime()),
            "initProperties(Ljava/util/Properties;)Ljava/util/Properties;" x1 value1(virt.Null),
            "identityHashCode(L//Object;)I" x1 ( vt => (x: Obj) => System.identityHashCode(x): virt.Val),
            "registerNatives()V" x noOp,
            "setIn0(Ljava/io/InputStream;)V" x1 noOp1,
            "setOut0(Ljava/io/PrintStream;)V" x1 noOp1,
            "setErr0(Ljava/io/PrintStream;)V" x1 noOp1
            ),
          "Thread"/(
            "currentThread()L//Thread;" x {vt => vt.obj},
            "isAlive()Z" x value(false: virt.Val),
            "setPriority0(I)V" x2 noOp2,
            "start0()V" x noOp
            ),
          "Throwable"/(
            "fillInStackTrace(I)L//Throwable;" x2 { vt => (throwable: Obj, dummy: virt.Int) =>
              import vt.vm;
              import vm._
              throwable.members(0)("stackTrace") =
                new virt.ObjArr(
                  imm.Type.Cls("java/lang/StackTraceElement"),
                  vt.getStackTrace.map { f =>
                    virt.Obj("java/lang/StackTraceElement",
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
              "isAlive()Z" x value(false: virt.Val)
              )
            )
          ),
        "security"/(
          "AccessController"/(
            "doPrivileged(L//PrivilegedAction;)L/lang/Object;" x1 {
              vt => (pa: Obj) =>
                vt.prepInvoke(pa.cls.clsData.tpe, "run", pa.cls.clsData.methods.find(_.name == "run").get.desc, Seq(pa))
            },
            "getStackAccessControlContext()L//AccessControlContext;" x { vt => virt.Obj("java/security/AccessControlContext")(vt.vm)},
            "getInheritedAccessControlContext()L//AccessControlContext;" x { vt => virt.Obj("java/security/AccessControlContext")(vt.vm)}
            )
          )
        ),
      "scala"/(
        "Predef$"/(
          "println(Ljava/lang/String;)V" x2 {
            vt => (x: virt.Obj, y: virt.Obj) => vt.vm.log("VIRTUAL " + virt.unvirtString(y))
          },
          "println(Ljava/lang/Object;)V" x2 {
            vt => (x: virt.Obj, y: virt.Obj) => vt.vm.log("VIRTUAL " + virt.unvirtString(y))
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
            "compareAndSwapInt(Ljava/lang/Object;JII)Z" x5 { vt => (unsafe: Any, a: virt.Obj, i: virt.Long, b: virt.Int, c: virt.Int) =>
              if (getObject(a, i) == b) {
                //putObject(a, i, b)
                true
              }else{
                false
              }
              true: virt.Val
            },
            "putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V" x4 {
              vt => (unsafe: virt.Obj, a: virt.Val, i: virt.Long, b: virt.Val) => putObject(a, i, b)
            },
            "getObject(Ljava/lang/Object;J)Ljava/lang/Object;" x3 {
              vt => (unsafe: virt.Obj, a: virt.Val, i: virt.Long) => getObject(a, i)
            },
            "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;" x3 {
              vt => (unsafe: virt.Obj, a: virt.Val, i: virt.Long) => getObject(a, i)
            },
            "getUnsafe()Lsun/misc/Unsafe;" x {vt =>
              import vt._
              virt.Obj("sun/misc/Unsafe")
            },
            "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z" x5 {
              vt => (unsafe: Obj, a: virt.Val, i: virt.Long, b: virt.Val, c: virt.Val) =>
                if (getObject(a, i) == b) {
                  putObject(a, i, b)
                  true
                }else{
                  false
                }
            },
            "objectFieldOffset(Ljava/lang/reflect/Field;)J" x2 { vt => (unsafe: Any, x: Obj) =>
              x(Type.Cls("java/lang/reflect/Field"), "slot").asInstanceOf[Int].toLong
            }
          ),
          "VM"/(
            "initialize()V" x noOp,
            "isBooted()Z" x value(true),
            "getSavedProperty(Ljava/lang/String;)Ljava/lang/String;" x1 {
              vt => (s: Obj) =>
                import vt.vm
                properties.get(s)
                          .map(x => x: virt.Val)
                          .getOrElse(virt.Null)
            }
          )

        ),

        "reflect"/(
          "NativeConstructorAccessorImpl"/(
            "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;" x2 {
              vt => (constr: Obj, args: Array[Obj]) =>
                import vt.vm; import vm._
                val cls: sm.Cls = imm.Type.Cls(constr.members(0)("clazz").asInstanceOf[virt.Cls].name)
                val newObj = new Obj(cls)
                vt.invoke(cls.clsData.tpe, "<init>", Type.Desc.read("()V"), Seq(newObj))
                newObj
            }
            ),
          "Reflection"/(
            "getCallerClass(I)Ljava/lang/Class;" x1 { vt => (n: virt.Int) =>
              import vt.vm; import vm._
              Type.Cls(vt.getStackTrace.drop(n).head.getClassName).obj
            },
            "getClassAccessFlags(Ljava/lang/Class;)I" x1 { vt => (x: virt.Cls) =>
              import vt.vm._;
              Type.Cls(x.name).clsData.access_flags
            },
            "registerMethodsToFilter(Ljava/lang/Class;[Ljava/lang/String;)V" x2 noOp2
          )
        )
      )
    ).toRoute().toMap
  }





}
