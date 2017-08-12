package metascala
package natives


import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.ByteBuffer

import collection.mutable
import metascala.rt
import imm.Type.Prim._
import metascala.imm.Desc
import metascala.imm.Type.Prim
import metascala.rt.Arr

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
        "nio"/(
          "charset"/(
            "Charset"/(
              "defaultCharset()Ljava/nio/charset/Charset;".func(I){ (vt) =>
                import vt.vm
                vt.invoke(
                  "metascala/DummyCharset",
                  imm.Sig("getValue", imm.Desc.read("()Ljava/nio/charset/Charset;")),
                  Agg.empty
                )
                val res = vt.returnedVal(0)

                res
              }
            )
          )
        ),
        "io"/(
          "UnixFileSystem"/(
            "initIDs()V".value(V)(())
          )
        ),
        "lang"/(
          "Class"/(
            "desiredAssertionStatus0(Ljava/lang/Class;)Z".value(I)(0),
            "desiredAssertionStatus()Z".value(I)(0),
            "forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;".func(I, I, I, I, I){
              (vt, name, boolean, classLoader, caller) =>
                import vt.vm
                val nameString = name.toRealObj[String]
                val tpe = imm.Type.readJava(nameString)
                try{
                  if (!nameString.contains("["))vm.ClsTable(tpe.asInstanceOf[imm.Type.Cls])
                  val x = vt.vm.typeObjCache(tpe)()
                  x
                } catch{case e: Exception =>
                  throw new java.lang.ClassNotFoundException(nameString)
                }

            },
            "getClassLoader0()Ljava/lang/ClassLoader;".value(I)(0),
            "getComponentType()Ljava/lang/Class;".func(I, I){ (vt, o) =>
              import vt.vm
              val obj = o.obj

              val oldName = obj("name").toRealObj[String]
              vt.vm.typeObjCache(imm.Type.Arr.readJava(oldName).innerType)()
            },

            "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;".func(I, I, I){ (vt, o, public) =>
              import vt.vm


              val obj = o.obj

              val name = obj("name").toRealObj[String]
              val cls = vm.ClsTable(name)
              val realFields = cls.fieldList ++ cls.staticList

              vm.alloc(implicit r =>
                rt.Arr.alloc("java/lang/reflect/Field",
                  realFields.zipWithIndex.map{ case (f, i) =>
                    rt.Obj.alloc("java/lang/reflect/Field",
                      "clazz" -> obj.address,
                      "slot" -> (if (f.static) cls.staticList else cls.fieldList).indexOf(f),
                      "name" -> vt.vm.internedStrings.getOrElseUpdate(f.name, f.name.toVirtObj),
                      "modifiers" -> f.access,
                      "type" -> vm.typeObjCache(f.desc)
                    ).address
                  }
                )
              )
              // if (f.static) cls.staticList else cls.fieldList).indexOf(f)
              // f.static(cls.staticList, cls.fieldList).indexOf(f)
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;".func(I, I, I){ (vt, o, bool) =>
              import vt.vm
              val clsObj = o.obj
              val clsName = clsObj("name").toRealObj[String]
              val cls = vm.ClsTable(clsName)
              val realMethods = cls.methods.filter(_.sig.name == "<init>")
              val vrtArr = vm.alloc(implicit r =>
                rt.Arr.alloc("java/lang/reflect/Constructor",
                  realMethods.zipWithIndex.map{ case (f, i) =>
                    rt.Obj.alloc("java/lang/reflect/Constructor",
                      "clazz" -> clsObj.address,
                      "slot" -> i,
                      "signature" -> f.sig.desc.unparse.toVirtObj,
                      "parameterTypes" -> rt.Arr.alloc("java/lang/Class",
                        f.sig.desc.args.map(t =>
                          vt.vm.typeObjCache(imm.Type.readJava(t.realCls.getName))
                        )
                      )
                    ).address
                  }
                )
              )
              vrtArr
            },
            "getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;".func(I, Z, I){ (vt, clsAddr, pub) =>
              import vt.vm
              val cls = vt.vm.ClsTable(clsAddr.obj.apply("name").toRealObj[String])
              vm.alloc(implicit r =>
                rt.Arr.alloc("java/lang/reflect/Method",
                  cls.methods.map{ m =>
                    rt.Obj.alloc("java/lang/reflect/Method").address
                  }
                )
              )()
            },
            "getEnclosingMethod0()[Ljava/lang/Object;".func(I, I){(vt, cls) => 0},
            "getDeclaringClass()Ljava/lang/Class;".func(I, I){ (vt, cls) => 0},
            "getInterfaces()[Ljava/lang/Class;".func(I, I){ (vt, clsAddr) =>
              import vt.vm
              val cls = vt.vm.ClsTable(clsAddr.obj.apply("name").toRealObj[String])
              vm.alloc(implicit r =>
                rt.Arr.alloc("java/lang/Class",
                  cls.typeAncestry
                     .filter(x => !cls.clsAncestry.contains(x))
                     .toSeq
                     .map(x => vt.vm.typeObjCache(x.cls.tpe))
                )
              )
            },
            "getModifiers()I".func(I, I){ (vt, o) =>
              import vt.vm
              val topClsName = o.obj.apply("name").toRealObj[String]

              vm.ClsTable(topClsName).accessFlags
            },
            "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;".func(I, I){ (vt, o) =>
              import vt.vm
              vt.vm.typeObjCache(imm.Type.readJava(o.toRealObj[String]))()
            },
            "getSuperclass()Ljava/lang/Class;".func(I, I){ (vt, o) =>
              import vt.vm
              val topClsName = o.obj.apply("name").toRealObj[String]

              vm.ClsTable(topClsName)
                .superType
                .map{_.name}
                .map(name => vt.vm.typeObjCache(imm.Type.readJava(name))())
                .getOrElse(0)


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
            "isInterface()Z".func(I, Z){ (vt, o) =>
              import vt.vm
              val clsObj = o.obj
              vm.ClsTable(clsObj("name").toRealObj[String]).isInterface
            },
            "isPrimitive()Z".func(I, I){ (vt, o) =>
              import vt.vm
              val clsObj = o.obj
              val name = clsObj("name").toRealObj[String]
              val res = Prim.allJava.contains(name)
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
              vt.vm.typeObjCache(imm.Type.readJava(name))()
            },
            "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;".func(I, I){ (vt, o) =>
              import vt.vm

              val name = o.toRealObj[String]
              val stream = getClass.getResourceAsStream("/" + name)
//              println("getSystemResourceAsStream " + name + " " + stream)
//              println(getClass.getClassLoader)
//              println("XXX " + name + " " + getClass.getResourceAsStream(name))
//              println("YYY " + name + " " + getClass.getResourceAsStream("/" + name))

              if (stream == null) 0
              else{
                val realResult = new DataInputStream(stream)

                val bytes = new Array[Byte](realResult.available())
                realResult.readFully(bytes)
                val byteStream = new ByteArrayInputStream(bytes)
                vm.alloc(byteStream.toVirtObj(_))
              }
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


              val string =
                if(value.isObj) value.obj.cls.tpe.javaName
                else value.arr.tpe.javaName

              vt.vm.typeObjCache(imm.Type.readJava(string))()
            },

            "hashCode()I".func(I, I){(vt, l) => l},
            "registerNatives()V".value(V){()}
          ),

          "Runtime"/(
            "freeMemory()J".value(J)(4*1024*1024),
            "availableProcessors()I".value(I)(1)
          ),

          "System"/(
            "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V".func(I, I, I, I, I, V){ (vt, src, srcIndex, dest, destIndex, length) =>
              val size = vt.vm.arrayTypeCache(vt.vm.heap(src)).size
              System.arraycopy(
                vt.vm.heap.memory,
                src + (srcIndex * size) + rt.Arr.headerSize,
                vt.vm.heap.memory,
                dest + (destIndex * size) + rt.Arr.headerSize,
                length * size
              )
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

              vm.currentThread
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
              throwable("stackTrace") = vm.alloc(vt.trace.toVirtObj(_))

              throwable.address()
            }
          ),
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;".func(I, I, I){ (vt, cls, length) =>
                import vt.vm
                val clsObj = cls.obj
                val clsName = clsObj("name").toRealObj[String]
                vm.alloc(rt.Arr.alloc(imm.Type.readJava(clsName), length)(_)).address()
              },
              "set(Ljava/lang/Object;ILjava/lang/Object;)V".func(I, I, I, V){ (vt, arr, index, obj) =>
                vt.invoke(
                  imm.Type.Cls("metascala/patches/java/lang/reflect/Array"),
                  imm.Sig("set", imm.Desc.read("(Ljava/lang/Object;ILjava/lang/Object;)V")),
                  Agg(arr, index, obj)
                )
              }
            ),
            "Constructor"/(
              "newInstance([Ljava/lang/Object;)Ljava/lang/Object;".func(I, I, I){
                (vt, constr, argArr) =>
                  import vt.vm
                  val cls = new rt.Obj(constr).apply("clazz")
                  val name = new rt.Obj(cls).apply("name").toRealObj[String].replace('.', '/')
                  val newObj = vm.alloc { implicit r =>
                    rt.Obj.alloc(name).address()
                  }

                  val descStr = new rt.Obj(constr).apply("signature").toRealObj[String]
                  val mRef = name.method(
                    "<init>",
                    Desc.read(descStr)
                  ).get

                  vt.invoke(
                    mRef,
                    Agg(newObj) ++ (if (argArr == 0) Seq() else Agg.from(argArr.arr))
                  )

                  newObj

              }
            ),
            "NativeConstructorAccessorImpl"/(
              "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;".func(I, I, I){
                (vt, cons, args) =>
                  import vt.vm
                  val name = cons.obj.apply("clazz").obj.apply("name").toRealObj[String]
                  vm.alloc(implicit r =>
                    rt.Obj.alloc(name)
                  ).address()
              }
            )
          ),
          "StrictMath"/(
            "log(D)D".func(D, D)( (vt, value) =>
              math.log(value)
            ),
            "pow(DD)D".func(D, D, D)( (vt, value, value2) =>
              math.pow(value, value2)
            )
          )
        ),

        "security"/(
          "AccessController"/(
            "doPrivileged(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;".func(I, I){ (vt, a) =>

              import vt.vm
              val pa = a.obj
              val mRef = vt.vm.resolveDirectRef(pa.cls.tpe, pa.cls.methods.find(_.sig.name == "run").get.sig).get
              var x = 0
              vt.invoke(mRef, Agg(pa.address()))

              vt.returnedVal(0)
            },
            "doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;".func(I, I){ (vt, a) =>

              import vt.vm
              val pa = a.obj
              val mRef = vt.vm.resolveDirectRef(pa.cls.tpe, pa.cls.methods.find(_.sig.name == "run").get.sig).get
              var x = 0
              val ret = vt.invoke(mRef, Agg(pa.address()))


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

            if (o.isObj){
              val thing = o.obj
              println("Virtual\t" + thing.address().toRealObj[Object])
            }else if(o.isArr){
              val s = Virtualizer.popVirtual(imm.Type.Arr(vt.vm.arrayTypeCache(vt.vm.heap(o + 1))), () => o)
              println("Virtual\t" + s)
            }else{
              println("Virtual\t" + null)
            }
          }
        )
      ),
      "sun"/(
        "misc"/(
          "Hashing"/(
            "randomHashSeed(Ljava/lang/Object;)I".value(I)(31337) // sufficiently random
          ),
          "Unsafe"/(
            "allocateMemory(J)J".func(I, J, J) { (vt, unsafe, size) =>
              val res = vt.vm.offHeapPointer
              vt.vm.offHeapPointer += size
              res
            },
            "freeMemory(J)V".func(I, J, V) { (vt, unsafe, offset) =>
              // Do nothing lol
              ()
            },
            "putLong(JJ)V".func(I, J, J, V) { (vt, unsafe, offset, value) =>
              val bs = ByteBuffer.allocate(8)
              bs.putLong(value)

              for(i <- 0 until 8) {
                vt.vm.offHeap(offset.toInt + i) = bs.get(i)
              }
              ()
            },
            "getByte(J)B".func(I, J, B) { (vt, unsafe, offset) =>
              val res = vt.vm.offHeap(offset.toInt)
              res
            },
            "arrayBaseOffset(Ljava/lang/Class;)I".value(I)(0),
            "arrayIndexScale(Ljava/lang/Class;)I".value(I)(1),
            "allocateInstance(Ljava/lang/Class;)Ljava/lang/Object;".func(I, I, I){ (vt, unsafe, clsPtr) =>
              import vt.vm
              val name = clsPtr.obj.apply("name").toRealObj[String]
              val x = vt.vm.alloc{ implicit r =>
                rt.Obj.alloc(name).address()
              }
              x
            },
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
            "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z".func(I, I, J, I, I, Z){ (vt, unsafe, o, slot, expected ,x) =>
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
            "ensureClassInitialized(Ljava/lang/Class;)V".func(I, I, V){ (vt, unsafe, cls) =>
              ()
            },
            "getObject(Ljava/lang/Object;J)Ljava/lang/Object;".func(I, I, J, I){ (vt, unsafe, o, offset) =>
              import vt.vm
              o.obj.members(offset.toInt)
            },
            "getBooleanVolatile(Ljava/lang/Object;J)Z".func(I, I, J, Z){ (vt, unsafe, o, offset) =>
              import vt.vm
              o.obj.members(offset.toInt) != 0
            },
            "putBooleanVolatile(Ljava/lang/Object;JZ)V".func(I, I, J, Z, V){ (vt, unsafe, o, offset, bool) =>
              import vt.vm
              Z.write(bool, o.obj.members(offset.toInt) = _)
            },
            "getByteVolatile(Ljava/lang/Object;J)B".func(I, I, J, B){ (vt, unsafe, o, offset) =>
              import vt.vm
              o.obj.members(offset.toInt).toByte
            },
            "putByteVolatile(Ljava/lang/Object;JB)V".func(I, I, J, B, V){ (vt, unsafe, o, offset, byte) =>
              import vt.vm
              B.write(byte, o.obj.members(offset.toInt) = _)
            },
            "getCharVolatile(Ljava/lang/Object;J)C".func(I, I, J, C){ (vt, unsafe, o, offset) =>
              import vt.vm
              o.obj.members(offset.toInt).toChar
            },
            "putCharVolatile(Ljava/lang/Object;JC)V".func(I, I, J, C, V){ (vt, unsafe, o, offset, char) =>
              import vt.vm
              C.write(char, o.obj.members(offset.toInt) = _)
            },
            "getInt(Ljava/lang/Object;J)I".func(I, I, J, I){ (vt, unsafe, o, offset) =>
              import vt.vm
              o.obj.members(offset.toInt)
            },
            "getIntVolatile(Ljava/lang/Object;J)I".func(I, I, J, I){ (vt, unsafe, o, offset) =>
              import vt.vm
              o.obj.members(offset.toInt)
            },
            "putInt(Ljava/lang/Object;JI)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, int) =>
              import vt.vm
              I.write(int, o.obj.members(offset.toInt) = _)
            },
            "getFloat(Ljava/lang/Object;J)F".func(I, I, J, F){ (vt, unsafe, o, offset) =>
              import vt.vm
              F.read(() => o.obj.members(offset.toInt))
            },
            "putFloat(Ljava/lang/Object;JF)V".func(I, I, J, F, V){ (vt, unsafe, o, offset, float) =>
              import vt.vm
              F.write(float, o.obj.members(offset.toInt) = _)
            },
            "getLongVolatile(Ljava/lang/Object;J)J".func(I, I, J, J){ (vt, unsafe, o, offset) =>
              import vt.vm
              J.read(reader(o.obj.members, offset.toInt))
            },
            "putLongVolatile(Ljava/lang/Object;JJ)V".func(I, I, J, J, V){ (vt, unsafe, o, offset, long) =>
              import vt.vm
              J.write(long, writer(o.obj.members, offset.toInt))
            },
            "getDouble(Ljava/lang/Object;J)D".func(I, I, J, D){ (vt, unsafe, o, offset) =>
              import vt.vm
              D.read(reader(o.obj.members, offset.toInt))
            },
            "putDouble(Ljava/lang/Object;JD)V".func(I, I, J, D, V){ (vt, unsafe, o, offset, double) =>
              import vt.vm
              D.write(double, writer(o.obj.members, offset.toInt))
            },
            "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;".func(I, I, J, I){ (vt, unsafe, o, offset) =>
              import vt.vm
              o.obj.members(offset.toInt)
            },
            "putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, ref) =>
              import vt.vm
              o.obj.members(offset.toInt) = ref
            },
            "putObject(Ljava/lang/Object;JLjava/lang/Object;)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, ref) =>
              import vt.vm
              o.obj.members(offset.toInt) = ref
            },
            "putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, ref) =>
              import vt.vm
              if (o.isObj)
                o.obj.members(offset.toInt) = ref
              else{
                o.arr(offset.toInt) = ref
              }
            },
            "objectFieldOffset(Ljava/lang/reflect/Field;)J".func(I, I, J){(vt, unsafe, f) =>
              import vt.vm
              val field = f.obj
              val s = field.apply("slot")
              s
            },
            "staticFieldOffset(Ljava/lang/reflect/Field;)J".func(I, I, J){ (vt, unsafe, f) =>
              import vt.vm
              val field = f.obj
              field.apply("slot")
            },
            "staticFieldBase(Ljava/lang/reflect/Field;)Ljava/lang/Object;".func(I, I, I){(vt, unsafe, f) =>
              import vt.vm
              vm.ClsTable(f.obj.apply("clazz").obj.apply("name").toRealObj[String]).statics.address()
            },
            "registerNatives()V".value(V)(()),
            "getUnsafe()Lsun/misc/Unsafe;".func(I){vt => vt.vm.theUnsafe.address()},
            "<clinit>()V".value(V)(())
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
              if (n >= vt.threadStack.length) 0
              else {
                val name = vt.threadStack(n).runningClass.name
                vt.vm.typeObjCache(imm.Type.readJava(name))()
              }
            },
            "getCallerClass()Ljava/lang/Class;".func(I){ (vt) =>
              import vt.vm
              val n = 1
              if (n >= vt.threadStack.length) 0
              else {
                val name = vt.threadStack(n).runningClass.name
                vt.vm.typeObjCache(imm.Type.readJava(name))()
              }
            },
            "getClassAccessFlags(Ljava/lang/Class;)I".func(I, I){ (vt, o) =>
              import vt.vm
              val addr = o.obj.apply("name")
              val str = addr.toRealObj[String]
              vm.ClsTable(str).accessFlags
            }
          )
        )
      ),
      "metascala"/(
        "Virtualizer$"/(
            "unsafe()Lsun/misc/Unsafe;".func(I){vt =>
              vt.vm.theUnsafe.address()
            }
        )
      )
    ).toRoute()
  }
}