package metascala
package natives


import java.io.{ByteArrayInputStream, DataInputStream}
import vrt.Arr
import collection.mutable
trait Default extends Bindings{

  val fileLoader = (name: String) => {
    val slashName = s"/$name"

    val loaded = getClass.getResourceAsStream(slashName)
    if (loaded == null) None
    else {
      val stream = new DataInputStream(loaded)
      val bytes = new Array[Byte](stream.available())
      stream.readFully(bytes)
      Some(bytes)
    }
  }

  val properties = Map[String, String]()
  val stack = mutable.Map.empty[String, Int]

  val trapped = {
    Seq(
      "java"/(
        "lang"/(
          "Class"/(
            "desiredAssertionStatus0(Ljava/lang/Class;)Z" x value(I)(0, 0),
            "forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;" x {vt =>
              import vt.vm
              val clsLoader = vt.pop
              val boolean = vt.pop
              val name = vt.pop
              vt.push(
                vrt.Obj.allocate("java/lang/Class",
                  "name" -> name
                ).address
              )
            },
            "getClassLoader0()Ljava/lang/ClassLoader;" x value(I)(1, 0),
            "getComponentType()Ljava/lang/Class;" x { vt =>
              import vt.vm
              val obj = vt.pop.obj
              val oldName = Virtualizer.popVirtual("java/lang/String", () => obj("name")).cast[String]
              val shortNewName = oldName.substring(1)
              val newName =
                if (Prim.all.keySet.map(""+_).contains(oldName))
                  Prim.all(shortNewName(0)).primClass.getName
                else
                  shortNewName

              val clsObj = vrt.Obj.allocate("java/lang/Class",
                "name" -> Virtualizer.pushVirtual(newName).apply(0)
              )
              vt.push(clsObj.address)
            },

            "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;" x {vt =>
              import vt.vm
              val public = vt.pop
              val obj = vt.pop.obj

              val name = Virtualizer.popVirtual("java/lang/String", () => obj("name")).cast[String]
              val realFields = vm.ClsTable(name).clsData.fields
              val vrtArr = vrt.Arr.allocate("java/lang/reflect/Field",
                realFields.zipWithIndex.map{ case (f, i) =>
                  vrt.Obj.allocate("java/lang/reflect/Field",
                    "clazz" -> obj.address,
                    "slot" -> i,
                    "name" -> stack.getOrElseUpdate(f.name, Virtualizer.pushVirtual(f.name).apply(0))
                  ).address
                }.toArray
              )

              vt.vm.log("MEMBERZ "+vrtArr)
              vt.push(vrtArr.address)


            },
            "getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;" x {vt =>
              ???
              //private native Method[]      getDeclaredMethods0(boolean publicOnly);
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" x {vt =>
              import vt.vm
              val bool = vt.pop
              val clsObj = vt.pop.obj
              val clsName = Virtualizer.popVirtual("java/lang/String", () => clsObj("name")).cast[String]
              val cls = vm.ClsTable(clsName)
              val realMethods = cls.clsData.methods.filter(_.name == "<init>")
              val vrtArr = vrt.Arr.allocate("java/lang/reflect/Constructor",
                realMethods .zipWithIndex.map{ case (f, i) =>
                  vrt.Obj.allocate("java/lang/reflect/Constructor",
                    "clazz" -> clsObj.address,
                    "slot" -> i,
                    "parameterTypes" -> vrt.Arr.allocate("java/lang/Class",
                      f.desc.args.map(t =>
                        vrt.Obj.allocate("java/lang/Class",
                          "name" -> Virtualizer.pushVirtual(t.realCls.getName).apply(0)
                        ).address
                      ).toArray
                    ).address
                  ).address
                }.toArray
              ).address
              vt.push(vrtArr)
              //private native Constructor<T>[] getDeclaredConstructors0(boolean publicOnly);
            },
            "getDeclaredClasses0(Z)[Ljava/lang/Class;" x {vt =>
              ???
              //private native Class<?>[]   getDeclaredClasses0();
            },
            "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;" x {vt =>
              import vt.vm
              val addr = vrt.Obj.allocate("java/lang/Class",
                "name" -> vt.pop
              ).address
              vt.push(addr)
            },
            "getSuperclass()Ljava/lang/Class;" x {vt =>
              import vt.vm
              val topClsName = Virtualizer.popVirtual("java/lang/String", () => vt.pop.obj.apply("name")).cast[String]
              vt.push(
                vm.ClsTable(topClsName)
                  .clsData
                  .superType
                  .map{_.name}
                  .map(name => vrt.Obj.allocate("java/lang/Class",
                      "name" -> Virtualizer.pushVirtual(name).apply(0)
                    ).address
                  ).getOrElse(0)
              )
            },

            "isArray()Z" x { vt =>
              import vt.vm
              val res =
                if(Virtualizer.popVirtual("java/lang/String", () => vt.pop.obj.apply("name")).cast[String].contains('[')) 1 else 0
              vt.push(res)
            },
            "isAssignableFrom(Ljava/lang/Class;)Z" x { vt =>
              import vt.vm
              val clsA = vt.pop.obj
              val clsB = vt.pop.obj
              val nameA = Virtualizer.popVirtual("java/lang/String", () => clsA.apply("name")).cast[String]
              val nameB = Virtualizer.popVirtual("java/lang/String", () => clsB.apply("name")).cast[String]

              vt.push(if (opcodes.Misc.check(imm.Type.read(nameA), imm.Type.read(nameB))) 1 else 0)
            },
            "isInterface()Z" x {vt =>
              import vt.vm
              val clsObj = vt.pop.obj
              vt.push(
                vm.ClsTable(
                  Virtualizer.popVirtual("java/lang/String", () => clsObj("name")).cast[String]
                ).clsData.access_flags & 0x0200
              )
            },
            "isPrimitive()Z" x {vt =>
              import vt.vm
              val clsObj = vt.pop.obj
              val res = Prim.all
                            .values
                            .map(_.primClass.getName)
                            .toList
                            .contains(Virtualizer.popVirtual("java/lang/String", () => clsObj("name")))
              vt.push(if (res) 1 else 0)
            },
            "registerNatives()V" x noOp(0)


          ),
          "ClassLoader"/(
            "getCaller(I)Ljava/lang/Class;" x {vt =>
              import vt.vm
              val name = vt.pop match{
                case 0 => "java/lang/ClassLoader"
                case 1 => vt.threadStack(0).runningClass.name
                case 2 => vt.threadStack(1).runningClass.name
              }
              vt.push(vrt.Obj.allocate("java/lang/Class", "name" -> Virtualizer.pushVirtual(name).apply(0)).address)
            },
            "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;" x {vt =>
              import vt.vm

              //public static InputStream getSystemResourceAsStream(String name) {
              val name = Virtualizer.popVirtual("java/lang/String", () => vt.pop).cast[String]
              val realResult = new DataInputStream(ClassLoader.getSystemResourceAsStream(name))
              val bytes = new Array[Byte](realResult.available())
              realResult.readFully(bytes)
              val byteStream = new ByteArrayInputStream(bytes)
              vt.push(Virtualizer.pushVirtual(byteStream).apply(0))
            },
            "registerNatives()V" x noOp(0)
          ),
          "Double"/(
            "doubleToRawLongBits(D)J" x noOp(0),
            "longBitsToDouble(J)D" x noOp(0)
          ),
          "Float"/(
            "intBitsToFloat(I)F" x noOp(0),
            "floatToRawIntBits(F)I" x noOp(0)
          ),
          "Object"/(
            "clone()Ljava/lang/Object;" x { vt =>

            },
            "getClass()Ljava/lang/Class;" x {vt =>

              import vt.vm
              val value = vt.pop

              val stringAddr = Virtualizer.pushVirtual(
                if(value.isObj) value.obj.cls.name.toDot
                else "[" + value.arr.innerType.name.toDot
              ).apply(0)
              vm.log("STRING ADDRESS " + stringAddr)
              val addr = vrt.Obj.allocate("java/lang/Class",
                "name" -> stringAddr
              ).address
              vt.push(addr)
            },
            "getName0()Ljava/lang/String;" x {vt =>
              vt.pop
              import vt.vm
              vm.ClsTable
              val addr = vrt.Obj.allocate("java/lang/Class",
                "name" -> 1337
              ).address
              vt.push(addr)
            },
            "hashCode()I" x noOp(0),
            "registerNatives()V" x noOp(0)
          ),

          "Runtime"/(
            "freeMemory()J" x value(J)(1, 4*1024*1024)
          ),

          "System"/(
            "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V" x { vt =>
              val Seq(src, srcIndex, dest, destIndex, length) = Seq(vt.pop, vt.pop, vt.pop, vt.pop, vt.pop).reverse
              System.arraycopy(vt.vm.Heap.memory, src + srcIndex + 2, vt.vm.Heap.memory, dest + destIndex + 2, length)
            },

            "identityHashCode(Ljava/lang/Object;)I" x { vt =>
              vt.push(vt.pop)
            },
            "nanoTime()J" x value(J)(0, System.nanoTime()),
            "currentTimeMillis()J" x value(J)(0, System.currentTimeMillis()),
            "getProperty(Ljava/lang/String;)Ljava/lang/String;" x value(I)(1, 0),
            "getProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;" x value(I)(2, 0),
            "registerNatives()V" x noOp(0)
          ),
          "String"/(
            "<clinit>()V" x noOp(0),
            "intern()Ljava/lang/String;" x {vt =>
              import vt.vm
              val addr = vt.pop
              val str = Virtualizer.popVirtual("java/lang/String", () => addr).cast[String]
              val result =  stack.getOrElseUpdate(str, addr)
              vt.push(result)
            }
          ),

          "Thread"/(
            "registerNatives()V" x noOp(0),
            "currentThread()Ljava/lang/Thread;" x {vt =>
              import vt.vm
              vt.push(vrt.Obj.allocate("java/lang/Thread",
                "group" -> vrt.Obj.allocate("java/lang/ThreadGroup").address,
                "priority" -> 5
              ).address)
            },
            "setPriority0(I)V" x noOp(1),
            "isAlive()Z" x value(Z)(1, false),
            "start0()V" x noOp(1)
          ),
          "Throwable"/(
            "fillInStackTrace()Ljava/lang/Throwable;" x {vt =>
              import vt.vm
              //vt.pop // pop dummy
              val throwable = vt.pop.obj
              val trace = vt.trace
              throwable("stackTrace") = Virtualizer.pushVirtual(vt.trace).apply(0)

              vt.push(throwable.address)
            }
          ),
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;" x {vt =>
                import vt.vm
                val length = vt.pop
                val clsObj = vt.pop.obj
                val clsName = Virtualizer.popVirtual("java/lang/String", () => clsObj("name")).cast[String]
                vt.push(vrt.Arr.allocate(clsName, length).address)
              }
              )
            )
        ),

        "security"/(
          "AccessController"/(
            "doPrivileged(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;" x {vt =>
              import vt.vm
              val pa = vt.pop.obj
              vm.log(vt.indent + "DOPRIVILEDGED " + pa.cls.clsData.tpe + "\t" + pa.address)
              val mRef = vt.vm.resolveDirectRef(pa.cls.clsData.tpe, pa.cls.clsData.methods.find(_.name == "run").get.sig).get
              vt.prepInvoke(mRef, Seq(pa.address))
            },
            "doPrivileged(L//PrivilegedAction;)L/lang/Object;" x { vt =>
              import vt.vm
              val pa = vt.pop.obj
              vm.log(vt.indent + "DOPRIVILEDGED " + pa.cls.clsData.tpe + "\t" + pa.address)
              val mRef = vt.vm.resolveDirectRef(pa.cls.clsData.tpe, pa.cls.clsData.methods.find(_.name == "run").get.sig).get
              vt.prepInvoke(mRef, Seq(pa.address))

            },
            "getStackAccessControlContext()Ljava/security/AccessControlContext;" x value(I)(0, 0)

          )
        ),
        "util"/(
          "Hashtable"/(
            "<clinit>()V" x noOp(0)
          )
        )
      ),
      "scala"/(
        "Predef$"/(
          "println(Ljava/lang/Object;)V" x { vt =>
            import vt.vm
            val thing = vt.pop.obj
            val predef = vt.pop
            println("Virtual\t" + Virtualizer.popVirtual("java/lang/Object", () => thing.address))
          }
        )
      ),
      "sun"/(
        "misc"/(
          "Unsafe"/(
            "arrayBaseOffset(Ljava/lang/Class;)I" x value(I)(2, 2),
            "arrayIndexScale(Ljava/lang/Class;)I" x value(I)(2, 1),
            "addressSize()I" x value(I)(1, 4),
            "compareAndSwapInt(Ljava/lang/Object;JII)Z" x {vt =>
              import vt.vm
              val x = vt.pop
              val expected = vt.pop
              val (a, b) = (vt.pop, vt.pop)
              val slot = J(b, a)
              val obj = vt.pop.obj
              val unsafe = vt.pop.obj

              val fieldName = obj.cls.clsData.fields(slot.toInt).name

              obj(fieldName) = x
              vt.push(1)
            },
            "objectFieldOffset(Ljava/lang/reflect/Field;)J" x {vt =>
              import vt.vm

              val field = vt.pop.obj
              val unsafe = vt.pop
              J.write(field.apply("slot"), vt.push)
            },
            "registerNatives()V" x noOp(0)
          ),
          "VM"/(
            "getSavedProperty(Ljava/lang/String;)Ljava/lang/String;" x value(I)(1, 0),
            "initialize()V" x noOp(0)
          )
        ),
        "reflect"/(
          "Reflection"/(
            "getCallerClass(I)Ljava/lang/Class;" x { vt =>
              import vt.vm
              val n = vt.pop
              val name = vt.threadStack(n).runningClass.name
              val clsObj = vrt.Obj.allocate("java/lang/Class",
                "name" -> Virtualizer.pushVirtual(name).apply(0)
              )
              vt.push(clsObj.address)
            },
            "getClassAccessFlags(Ljava/lang/Class;)I" x {vt =>
              import vt.vm
              val addr = vt.pop.obj.apply("name")
              val str = Virtualizer.popVirtual("java/lang/String", () => addr).cast[String]
              vt.push(vm.ClsTable(str).clsData.access_flags)
            }
          )
        )
      )

    ).toRoute()
  }
}