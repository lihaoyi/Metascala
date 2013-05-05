package metascala
package natives


import java.io.{ByteArrayInputStream, DataInputStream}
import vrt.Arr
import collection.mutable
import metascala.vrt
import metascala.vrt

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
            "desiredAssertionStatus0(Ljava/lang/Class;)Z" x value(I)(0, 0),
            "forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;)Ljava/lang/Class;" x {vt =>
              /*import vt.vm
              val clsLoader = vt.pop
              val boolean = vt.pop
              val name = vt.pop
              vt.push(
                "java/lang/Class".allocObj(
                  "name" -> name
                )
              )*/
            },
            "getClassLoader0()Ljava/lang/ClassLoader;" x value(I)(1, 0),
            "getComponentType()Ljava/lang/Class;" x { vt =>
              /*import vt.vm
              val obj = vt.pop.obj
              val oldName = obj("name").toRealObj[String]
              val shortNewName = oldName.substring(1)
              val newName =
                if (Prim.all.keySet.map(""+_).contains(oldName))
                  Prim.all(shortNewName(0)).primClass.getName
                else
                  shortNewName

              val clsObj = "java/lang/Class".allocObj(
                "name" -> newName.toVirtObj
              )
              vt.push(clsObj)*/
            },

            "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;" x {vt =>
              /*import vt.vm
              val public = vt.pop
              val obj = vt.pop.obj

              val name = obj("name").toRealObj[String]
              val realFields = vm.ClsTable(name).clsData.fields

              val vrtArr =
                "java/lang/reflect/Field".allocArr(
                  realFields.zipWithIndex.map{ case (f, i) =>
                   "java/lang/reflect/Field".allocObj(
                      "clazz" -> obj.address,
                      "slot" -> i,
                      "name" -> vt.vm.internedStrings.getOrElseUpdate(f.name, f.name.toVirtObj)
                    )
                  }
                )

              vt.push(vrtArr)*/
            },
            "getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;" x {vt =>
              ???              //private native Method[]      getDeclaredMethods0(boolean publicOnly);
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;" x {vt =>
              /*import vt.vm
              val bool = vt.pop
              val clsObj = vt.pop.obj
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
              vt.push(vrtArr)*/
            },
            "getDeclaredClasses0(Z)[Ljava/lang/Class;" x {vt =>
              ???
              //private native Class<?>[]   getDeclaredClasses0();
            },
            "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;" x {vt =>
             /* import vt.vm
              val addr = "java/lang/Class".allocObj(
                "name" -> vt.pop
              )
              vt.push(addr)*/
            },
            "getSuperclass()Ljava/lang/Class;" x {vt =>
              /*import vt.vm
              val topClsName = vt.pop.obj.apply("name").toRealObj[String]
              vt.push(
                vm.ClsTable(topClsName)
                  .clsData
                  .superType
                  .map{_.name}
                  .map(name =>
                    "java/lang/Class".allocObj(
                      "name" -> name.toVirtObj
                    )
                  ).getOrElse(0)
              )*/
            },

            "isArray()Z" x { vt =>
              /*import vt.vm
              val res =
                if(vt.pop.obj.apply("name").toRealObj[String].contains('[')) 1 else 0
              vt.push(res)*/
            },
            "isAssignableFrom(Ljava/lang/Class;)Z" x { vt =>
              /*import vt.vm
              val clsA = vt.pop.obj
              val clsB = vt.pop.obj
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
              vt.push(if (check(imm.Type.read(nameA), imm.Type.read(nameB))) 1 else 0)*/
            },
            "isInterface()Z" x {vt =>
              /*import vt.vm
              val clsObj = vt.pop.obj
              vt.push(
                vm.ClsTable(
                  clsObj("name").toRealObj[String]
                ).clsData.access_flags & 0x0200
              )*/
            },
            "isPrimitive()Z" x {vt =>
              /*import vt.vm
              val clsObj = vt.pop.obj
              val res = Prim.all
                            .values
                            .map(_.primClass.getName)
                            .toList
                            .contains(clsObj("name").toRealObj[String])
              vt.push(if (res) 1 else 0)*/
            },
            "registerNatives()V" x noOp(0)
          ),
          "ClassLoader"/(
            "getCaller(I)Ljava/lang/Class;" x {vt =>
              /*import vt.vm
              val name = vt.pop match{
                case 0 => "java/lang/ClassLoader"
                case 1 => vt.threadStack(0).runningClass.name
                case 2 => vt.threadStack(1).runningClass.name
              }
              vt.push("java/lang/Class".allocObj("name" -> name.toVirtObj))*/
            },
            "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;" x {vt =>
              /*import vt.vm

              val name = vt.pop.toRealObj[String]
              val realResult = new DataInputStream(ClassLoader.getSystemResourceAsStream(name))
              val bytes = new Array[Byte](realResult.available())
              realResult.readFully(bytes)
              val byteStream = new ByteArrayInputStream(bytes)
              vt.push(byteStream.toVirtObj)*/
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
            "clone()Ljava/lang/Object;" x noOp(0),
            "getClass()Ljava/lang/Class;" x {vt =>
/*
              import vt.vm
              val value = vt.pop

              val stringAddr = (
                if(value.isObj) value.obj.cls.name.toDot
                else "[" + value.arr.innerType.name.toDot
              ).toVirtObj

              val addr = "java/lang/Class".allocObj(
                "name" -> stringAddr
              )
              vt.push(addr)*/
            },
            "getName0()Ljava/lang/String;" x {vt =>
              /*vt.pop
              import vt.vm
              vm.ClsTable
              val addr = "java/lang/Class".allocObj(
                "name" -> 1337
              )
              vt.push(addr)*/
            },
            "hashCode()I" x noOp(0),
            "registerNatives()V" x noOp(0)
          ),

          "Runtime"/(
            "freeMemory()J" x value(J)(1, 4*1024*1024)
          ),

          "System"/(
            "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V" x { vt =>
              /*val Seq(src, srcIndex, dest, destIndex, length) = Seq(vt.pop, vt.pop, vt.pop, vt.pop, vt.pop).reverse
              System.arraycopy(vt.vm.Heap.memory, src + srcIndex + 2, vt.vm.Heap.memory, dest + destIndex + 2, length)*/
            },

            "identityHashCode(Ljava/lang/Object;)I" x { vt =>
              /*vt.push(vt.pop)*/
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
              /*import vt.vm
              val addr = vt.pop
              val str = addr.toRealObj[String]
              val result = vt.vm.internedStrings.getOrElseUpdate(str, addr)
              vt.push(result)*/
            }
          ),

          "Thread"/(
            "registerNatives()V" x noOp(0),
            "currentThread()Ljava/lang/Thread;" x {vt =>
              /*import vt.vm
              vt.push(
                "java/lang/Thread".allocObj(
                  "group" -> "java/lang/ThreadGroup".allocObj(),
                  "priority" -> 5
                )
              )*/
            },
            "setPriority0(I)V" x noOp(1),
            "isAlive()Z" x value(Z)(1, false),
            "start0()V" x noOp(1)
          ),
          "Throwable"/(
            "fillInStackTrace()Ljava/lang/Throwable;" x {vt =>
              /*import vt.vm
              //vt.pop // pop dummy
              val throwable = vt.pop.obj
              val trace = vt.trace
              throwable("stackTrace") = vt.trace.toVirtObj

              vt.push(throwable.address)*/
            }
          ),
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;" x {vt =>
                /*import vt.vm
                val length = vt.pop
                val clsObj = vt.pop.obj
                val clsName = clsObj("name").toRealObj[String]
                vt.push(vrt.Arr.allocate(clsName, length).address)*/
              }
            )
          )
        ),

        "security"/(
          "AccessController"/(
            "doPrivileged(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;" x {vt =>
              /*import vt.vm
              val pa = vt.pop.obj
              val mRef = vt.vm.resolveDirectRef(pa.cls.clsData.tpe, pa.cls.clsData.methods.find(_.name == "run").get.sig).get
              vt.prepInvoke(mRef, Seq(pa.address))*/
            },
            "doPrivileged(L//PrivilegedAction;)L/lang/Object;" x { vt =>
              /*import vt.vm
              val pa = vt.pop.obj
              val mRef = vt.vm.resolveDirectRef(pa.cls.clsData.tpe, pa.cls.clsData.methods.find(_.name == "run").get.sig).get
              vt.prepInvoke(mRef, Seq(pa.address))*/

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
            /*import vt.vm
            val thing = vt.pop.obj
            val predef = vt.pop
            println("Virtual\t" + thing.address.toRealObj[Object])*/
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
              /*import vt.vm
              val x = vt.pop
              val expected = vt.pop
              val (a, b) = (vt.pop, vt.pop)
              val slot = J(b, a)
              val obj = vt.pop.obj
              val unsafe = vt.pop.obj

              val fieldName = obj.cls.clsData.fields(slot.toInt).name

              obj(fieldName) = x
              vt.push(1)*/
            },
            "objectFieldOffset(Ljava/lang/reflect/Field;)J" x {vt =>
              /*import vt.vm
              val field = vt.pop.obj
              val unsafe = vt.pop
              J.write(field.apply("slot"), vt.push)*/
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
              /*import vt.vm
              val n = vt.pop
              val name = vt.threadStack(n).runningClass.name
              val clsObj = "java/lang/Class".allocObj(
                "name" -> name.toVirtObj
              )
              vt.push(clsObj)*/
            },
            "getClassAccessFlags(Ljava/lang/Class;)I" x {vt =>
              /*import vt.vm
              val addr = vt.pop.obj.apply("name")
              val str = addr.toRealObj[String]
              vt.push(vm.ClsTable(str).clsData.access_flags)*/
            }
          )
        )
      )
    ).toRoute()
  }
}