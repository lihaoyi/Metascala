package metascala
package natives


import java.io.{ByteArrayInputStream, DataInputStream}
import vrt.Arr
import collection.mutable
import metascala.vrt
import metascala.vrt
import imm.Type.Prim._
import metascala.imm.Type.Prim

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


  val trapped = {
    Seq(
      "java"/(
        "lang"/(
          "Class"/(
            "desiredAssertionStatus0(Ljava/lang/Class;)Z".value(I)(0),
            "desiredAssertionStatus()Z".value(I)(0),
            "forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;".func(I, I, I, I){
              (vt, clsloader, boolean, name) =>
                import vt.vm
                "java/lang/Class".allocObj(
                  "name" -> name
                )
            },
            "getClassLoader0()Ljava/lang/ClassLoader;".value(I)(0),
            "getComponentType()Ljava/lang/Class;".func(I, I){ (vt, o) =>
              import vt.vm
              val obj = o.obj
              val oldName = obj("name").toRealObj[String]
              val shortNewName = oldName.substring(1)
              val newName =
                if (Prim.all.keySet.map(""+_).contains(oldName))
                  Prim.all(shortNewName(0)).primClass.getName
                else
                  shortNewName

              "java/lang/Class".allocObj(
                "name" -> newName.toVirtObj
              )
            },

            "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;".func(I, I, I){ (vt, o, public) =>
              import vt.vm

              val obj = o.obj

              val name = obj("name").toRealObj[String]
              val realFields = vm.ClsTable(name).fieldList


              "java/lang/reflect/Field".allocArr(
                realFields.zipWithIndex.map{ case (f, i) =>
                  "java/lang/reflect/Field".allocObj(
                    "clazz" -> obj.address,
                    "slot" -> i,
                    "name" -> vt.vm.internedStrings.getOrElseUpdate(f.name, f.name.toVirtObj)
                  )
                }
              )

            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;".func(I, I, I){ (vt, bool, o) =>
              import vt.vm

              val clsObj = o.obj
              val clsName = clsObj("name").toRealObj[String]
              val cls = vm.ClsTable(clsName)
              val realMethods = cls.clsData.methods.filter(_.name == "<init>")
              val vrtArr = "java/lang/reflect/Constructor".allocArr(
                realMethods .zipWithIndex.map{ case (f, i) =>
                  "java/lang/reflect/Constructor".allocObj(
                    "clazz" -> clsObj.address,
                    "slot" -> i,
                    "parameterTypes" -> "java/lang/Class".allocArr(
                      f.desc.args.map(t =>
                        "java/lang/Class".allocObj(
                          "name" -> t.realCls.getName.toVirtObj
                        )
                      )
                    )
                  )
                }
              )
              vrtArr
            },
            "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;".func(I, I){ (vt, o) =>
              import vt.vm
              val addr = "java/lang/Class".allocObj(
                "name" -> o
              )
              addr
            },
            "getSuperclass()Ljava/lang/Class;".func(I, I){ (vt, o) =>
              import vt.vm
              val topClsName = o.obj.apply("name").toRealObj[String]

              vm.ClsTable(topClsName)
                .clsData
                .superType
                .map{_.name}
                .map(name =>
                  "java/lang/Class".allocObj(
                    "name" -> name.toVirtObj
                  )
                ).getOrElse(0)

            },

            "isArray()Z".func(I, I){ (vt, o) =>
              import vt.vm

              if(o.obj.apply("name").toRealObj[String].contains('[')) 1 else 0

            },
            "isAssignableFrom(Ljava/lang/Class;)Z".func(I, I, I){ (vt, a, b) =>
              import vt.vm
              val clsA = a.obj
              val clsB = b.obj
              val nameA = clsA("name").toRealObj[String]
              val nameB = clsB("name").toRealObj[String]

              def check(s: imm.Type, t: imm.Type)(implicit vm: VM): Boolean = {

                (s, t) match{

                  case (s: imm.Type.Cls, t: imm.Type.Cls) => s.cls.typeAncestry.contains(t)
                  case (s: imm.Type.Arr, imm.Type.Cls("java/lang/Object")) => true
                  case (s: imm.Type.Arr, imm.Type.Cls("java/lang/Cloneable")) => true
                  case (s: imm.Type.Arr, imm.Type.Cls("java/io/Serializable")) => true
                  case (imm.Type.Arr(imm.Type.Prim(a)), imm.Type.Arr(imm.Type.Prim(b))) => a == b
                  case (imm.Type.Arr(sc: imm.Type), imm.Type.Arr(tc: imm.Type)) => check(sc, tc)
                  case _ => false
                }
              }
              if (check(imm.Type.read(nameA.replace('.', '/')), imm.Type.read(nameB.replace('.', '/')))) 1 else 0
            },
            "isInterface()Z".func(I, I){ (vt, o) =>
              import vt.vm
              val clsObj = o.obj
              vm.ClsTable(
                clsObj("name").toRealObj[String]
              ).clsData.access_flags & 0x0200
            },
            "isPrimitive()Z".func(I, I){ (vt, o) =>
              import vt.vm
              val clsObj = o.obj
              val res = Prim.all
                            .values
                            .map(_.primClass.getName)
                            .toList
                            .contains(clsObj("name").toRealObj[String])
              if (res) 1 else 0
            },
            "registerNatives()V".value(V)(())
          ),
          "ClassLoader"/(
            "getCaller(I)Ljava/lang/Class;".func(I, I){ (vt, o) =>
              import vt.vm
              val name = o match{
                case 0 => "java/lang/ClassLoader"
                case 1 => vt.threadStack(0).runningClass.name
                case 2 => vt.threadStack(1).runningClass.name
              }
              "java/lang/Class".allocObj("name" -> name.toVirtObj)
            },
            "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;".func(I, I){ (vt, o) =>
              import vt.vm

              val name = o.toRealObj[String]
              val realResult = new DataInputStream(ClassLoader.getSystemResourceAsStream(name))
              val bytes = new Array[Byte](realResult.available())
              realResult.readFully(bytes)
              val byteStream = new ByteArrayInputStream(bytes)
              byteStream.toVirtObj
            },
            "registerNatives()V".value(V)(())
          ),
          "Double"/(
            "doubleToRawLongBits(D)J".func(J, J){(vt, l) => l},
            "longBitsToDouble(J)D".func(J, J){(vt, l) => l}
          ),
          "Float"/(
            "intBitsToFloat(I)F".func(I, I){(vt, l) => l},
            "floatToRawIntBits(F)I".func(I, I){(vt, l) => l}
          ),
          "Object"/(
            "clone()Ljava/lang/Object;".func(I, I){(vt, l) => l},
            "getClass()Ljava/lang/Class;".func(I, I){ (vt, value) =>

              import vt.vm


              val stringAddr = (
                if(value.isObj) value.obj.cls.name.toDot
                else "[" + value.arr.innerType.name.toDot
              ).toVirtObj

              val addr = "java/lang/Class".allocObj(
                "name" -> stringAddr
              )
              addr
            },

            "hashCode()I".func(I, I){(vt, l) => l},
            "registerNatives()V".value(V){()}
          ),

          "Runtime"/(
            "freeMemory()J".value(J)(4*1024*1024)
          ),

          "System"/(
            "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V".func(I, I, I, I, I, V){ (vt, src, srcIndex, dest, destIndex, length) =>
              System.arraycopy(vt.vm.heap.memory, src + srcIndex + vrt.Arr.headerSize, vt.vm.heap.memory, dest + destIndex + vrt.Arr.headerSize, length)
            },

            "identityHashCode(Ljava/lang/Object;)I".func(I, I){(vt, l) => l},
            "nanoTime()J".value(J)(System.nanoTime()),
            "currentTimeMillis()J".value(J)(System.currentTimeMillis()),
            "getProperty(Ljava/lang/String;)Ljava/lang/String;".value(I)(0),
            "getProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;".value(I)(0),
            "registerNatives()V".value(V)(())
          ),
          "String"/(
            "intern()Ljava/lang/String;".func(I, I){ (vt, addr) =>
              import vt.vm
              val str = addr.toRealObj[String]
              val result = vt.vm.internedStrings.getOrElseUpdate(str, addr)
              result
            }
          ),

          "Thread"/(
            "registerNatives()V".value(V)(()),
            "currentThread()Ljava/lang/Thread;".func(I){ vt =>
              import vt.vm

              "java/lang/Thread".allocObj(
                "group" -> "java/lang/ThreadGroup".allocObj(),
                "priority" -> 5
              )
            },
            "setPriority0(I)V".value(V)(()),
            "isAlive()Z".value(Z)(false),
            "start0()V".value(V)(())
          ),
          "Throwable"/(
            "fillInStackTrace()Ljava/lang/Throwable;".func(I, I){ (vt, addr) =>
              import vt.vm
              val throwable = addr.obj
              val trace = vt.trace
              throwable("stackTrace") = vt.trace.toVirtObj

              throwable.address
            }
          ),
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;".func(I, I, I){ (vt, cls, length) =>
                import vt.vm
                val clsObj = cls.obj
                val clsName = clsObj("name").toRealObj[String]
                vrt.Arr.allocate(clsName.replace('.', '/'), length).address
              }
            )
          )
        ),

        "security"/(
          "AccessController"/(
            "doPrivileged(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;".func(I, I){ (vt, a) =>

              import vt.vm
              val pa = a.obj
              val mRef = vt.vm.resolveDirectRef(pa.cls.clsData.tpe, pa.cls.clsData.methods.find(_.name == "run").get.sig).get
              var x = 0
              vt.invoke(mRef, Seq(pa.address))

              vt.returnedVal(0)
            },
            "doPrivileged(L//PrivilegedAction;)L/lang/Object;".func(I, I){ (vt, a) =>

              import vt.vm
              val pa = a.obj
              val mRef = vt.vm.resolveDirectRef(pa.cls.clsData.tpe, pa.cls.clsData.methods.find(_.name == "run").get.sig).get
              var x = 0
              val ret = vt.invoke(mRef, Seq(pa.address))


              vt.returnedVal(0)
            },
            "getStackAccessControlContext()Ljava/security/AccessControlContext;".value(I)(0)

          )
        ),
        "util"/(
          "concurrent"/(
            "atomic"/(
              "AtomicLong"/(
                "VMSupportsCS8()Z".func(Z){ vt => true }
              )
            )
          )
        )
      ),
      "scala"/(
        "Predef$"/(
          "println(Ljava/lang/Object;)V".func(I, I, V){ (vt, predef, o) =>
            import vt.vm
            val thing = o.obj
            println("Virtual\t" + thing.address.toRealObj[Object])
          }
        )
      ),
      "sun"/(
        "misc"/(

          "Unsafe"/(
            "arrayBaseOffset(Ljava/lang/Class;)I".value(I)(2),
            "arrayIndexScale(Ljava/lang/Class;)I".value(I)(1),
            "addressSize()I".value(I)(4),
            "compareAndSwapInt(Ljava/lang/Object;JII)Z".func(I, I, J, I, I, Z){ (vt, unsafe, o, slot, expected ,x) =>
              import vt.vm
              val obj = o.obj
              if (obj.members(slot.toInt) == expected){
                obj.members(slot.toInt) = x
                true
              }else{
                false
              }

            },
            "compareAndSwapLong(Ljava/lang/Object;JJJ)Z".func(I, I, J, J, J, Z){ (vt, unsafe, o, slot, expected ,x) =>
              import vt.vm
              val obj = o.obj
              val current = J.read(reader(obj.members, slot.toInt))
              if (current == expected){
                J.write(x, writer(obj.members, slot.toInt))
                true
              }else{
                false
              }

            },

            "objectFieldOffset(Ljava/lang/reflect/Field;)J".func(I, I, I){(vt, unsafe, f) =>
              import vt.vm
              val field = f.obj
              println("objectFieldOffset " + field.apply("slot"))
              field.apply("slot")
            },

            "registerNatives()V".value(V)(()),
            "getUnsafe()Lsun/misc/Unsafe;".func(I){vt => vt.vm.theUnsafe.address}
          ),
          "VM"/(
            "getSavedProperty(Ljava/lang/String;)Ljava/lang/String;".value(I)(0),
            "initialize()V".value(V)(())

          )
        ),
        "reflect"/(
          "Reflection"/(
            "filterFields(Ljava/lang/Class;[Ljava/lang/reflect/Field;)[Ljava/lang/reflect/Field;".func(I, I, I){ (vt, cls, fs) =>
              fs
            },
            "getCallerClass(I)Ljava/lang/Class;".func(I, I){ (vt, n) =>
              import vt.vm
              val name = vt.threadStack(n).runningClass.name
              val clsObj = "java/lang/Class".allocObj(
                "name" -> name.toVirtObj
              )
              clsObj
            },
            "getClassAccessFlags(Ljava/lang/Class;)I".func(I, I){ (vt, o) =>
              import vt.vm
              val addr = o.obj.apply("name")
              val str = addr.toRealObj[String]
              vm.ClsTable(str).clsData.access_flags
            }
          )
        )
      )
    ).toRoute()
  }
}