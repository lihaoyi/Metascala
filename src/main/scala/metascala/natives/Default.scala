package metascala
package natives


import java.io.DataInputStream
import vrt.Arr

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


  val trapped = {
    Seq(
      "java"/(
        "lang"/(
          "Class"/(
            "desiredAssertionStatus0(Ljava/lang/Class;)Z" x value(I)(0, 0),
            "getClassLoader0()Ljava/lang/ClassLoader;" x value(I)(1, 0),
            "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;" x {vt =>
              import vt.vm
              val public = vt.pop
              val obj = vt.pop.obj

              val name = Virtualizer.popVirtual("java/lang/String", () => obj("name"))
              println()
              vrt.Arr.allocate("java/lang/reflect/Field",
                obj.cls.clsData.fields.zipWithIndex.map{ case (f, i) =>
                  vrt.Obj.allocate("java/lang/reflect/Field",
                    "clazz" -> obj.address,
                    "slot" -> i,
                    "name" -> Virtualizer.pushVirtual(f.name).apply(0)
                  ).address
                }.toArray
              )
              val newAddr = Virtualizer.pushVirtual(Class.forName(name.cast[String].toDot).getDeclaredFields).apply(0)
              println("newAddr " + newAddr)
              vt.push(newAddr)
            },
            "getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;" x {vt =>
              //private native Method[]      getDeclaredMethods0(boolean publicOnly);
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" x {vt =>
              //private native Constructor<T>[] getDeclaredConstructors0(boolean publicOnly);
            },
            "getDeclaredClasses0(Z)[Ljava/lang/Class;" x {vt =>
              //private native Class<?>[]   getDeclaredClasses0();
            },
            "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;" x {vt =>
              import vt.vm
              val addr = vrt.Obj.allocate("java/lang/Class",
                "name" -> vt.pop
              ).address
              println("Allocating Cls " + addr)
              vt.push(addr)
            },
            "registerNatives()V" x noOp(0)

          ),
          "ClassLoader"/(
            "registerNatives()V" x noOp(0)
          ),
          "Double"/(
            "doubleToRawLongBits(D)J" x noOp(0)
          ),
          "Float"/(
            "intBitsToFloat(I)F" x noOp(0),
            "floatToRawIntBits(F)I" x noOp(0)
          ),
          "Object"/(
            "getClass()Ljava/lang/Class;" x {vt =>

              import vt.vm
              val obj = vt.pop.obj
              val stringAddr = Virtualizer.pushVirtual(obj.cls.name.toDot).apply(0)

              val addr = vrt.Obj.allocate("java/lang/Class",
                "name" -> stringAddr
              ).address
              println("Allocating Cls " + addr)
              vt.push(addr)
            },
            "getName0()Ljava/lang/String;" x {vt =>
              vt.pop
              import vt.vm
              vm.ClsTable
              val addr = vrt.Obj.allocate("java/lang/Class",
                "name" -> 1337
              ).address
              println("Allocating Cls " + addr)
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
            "currentTimeMillis()J" x value(J)(0, System.currentTimeMillis())
          ),
          "String"/(
            "<clinit>()V" x noOp(0),
            "intern()Ljava/lang/String;" x noOp(0)
          ),
          "System"/(
            "registerNatives()V" x noOp(0)
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
          )

        ),
        "security"/(
          "AccessController"/(
            "doPrivileged(L//PrivilegedAction;)L/lang/Object;" x { vt =>
              import vt.vm
              val pa = vt.pop.obj
              vt.prepInvoke(pa.cls.clsData.tpe, pa.cls.clsData.methods.find(_.name == "run").get.sig, Seq(pa))
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
      "sun"/(
        "misc"/(
          "Unsafe"/(
            "arrayBaseOffset(Ljava/lang/Class;)I" x value(I)(2, 2),
            "arrayIndexScale(Ljava/lang/Class;)I" x value(I)(2, 1),
            "addressSize()I" x value(I)(1, 4),
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
              println("Allocating Cls " + clsObj.address)
              vt.push(clsObj.address)

            }
          )
        )
      )

    ).toRoute()
  }
}