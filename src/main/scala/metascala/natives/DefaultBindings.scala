package metascala
package natives


import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.ByteBuffer

import imm.Type.Prim._
import metascala.imm.Type.Prim
import metascala.util.{Agg, Constants, Util}

object DefaultBindings extends Bindings{


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
                
                vt.invoke(
                  imm.Type.Cls("metascala/DummyCharset"),
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
            "getProtectionDomain0()Ljava/security/ProtectionDomain;".value(I)(0),
            "desiredAssertionStatus0(Ljava/lang/Class;)Z".value(I)(0),
            "desiredAssertionStatus()Z".value(I)(0),
            "forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;".func(I, I, I, I, I){
              (vt, name, boolean, classLoader, caller) =>
                
                val nameString = vt.toRealObj[String](name)
                val tpe = imm.Type.readJava(nameString)
                try{
                  if (!nameString.contains("["))vt.ClsTable(tpe.asInstanceOf[imm.Type.Cls])
                  val x = vt.typeObjCache(tpe)()
                  x
                } catch{case e: Exception =>
                  throw new java.lang.ClassNotFoundException(nameString)
                }

            },
            "getClassLoader0()Ljava/lang/ClassLoader;".value(I)(0),
            "getComponentType()Ljava/lang/Class;".func(I, I){ (vt, o) =>
              
              val obj = vt.obj(o)

              val oldName = vt.toRealObj[String](obj("name"))
              vt.typeObjCache(imm.Type.Arr.readJava(oldName).innerType)()
            },

            "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;".func(I, I, I){ (vt, o, public) =>
              


              val obj = vt.obj(o)

              val name = vt.toRealObj[String](obj("name"))
              val cls = vt.ClsTable(imm.Type.Cls(name))
              val realFields = cls.fieldList ++ cls.staticList

              vt.alloc(implicit r =>
                rt.Obj.allocArr(imm.Type.Cls("java/lang/reflect/Field"),
                  realFields.zipWithIndex.map{ case (f, i) =>
                    rt.Obj.alloc(vt.ClsTable(imm.Type.Cls("java/lang/reflect/Field")),
                      "clazz" -> obj.address,
                      "slot" -> (if (f.static) cls.staticList else cls.fieldList).indexOf(f),
                      "name" -> vt.internedStrings.getOrElseUpdate(f.name, vt.toVirtObj(f.name)),
                      "modifiers" -> f.access,
                      "type" -> vt.typeObjCache(f.desc)
                    ).address
                  }
                )
              )
              // if (f.static) cls.staticList else cls.fieldList).indexOf(f)
              // f.static(cls.staticList, cls.fieldList).indexOf(f)
            },
            "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;".func(I, I, I){ (vt, o, bool) =>
              
              val clsObj = vt.obj(o)
              val clsName = vt.toRealObj[String](clsObj("name"))
              val cls = vt.ClsTable(imm.Type.Cls(clsName))
              val realMethods = cls.methods.filter(_.sig.name == "<init>")
              val vrtArr = vt.alloc(implicit r =>
                rt.Obj.allocArr(imm.Type.Cls("java/lang/reflect/Constructor"),
                  realMethods.zipWithIndex.map{ case (f, i) =>
                    rt.Obj.alloc(vt.ClsTable(imm.Type.Cls("java/lang/reflect/Constructor")),
                      "clazz" -> clsObj.address,
                      "slot" -> i,
                      "signature" -> vt.toVirtObj(f.sig.desc.unparse),
                      "parameterTypes" -> rt.Obj.allocArr(imm.Type.Cls("java/lang/Class"),
                        f.sig.desc.args.map(t =>
                          vt.typeObjCache(imm.Type.readJava(t.realCls.getName))
                        )
                      )
                    ).address
                  }
                )
              )
              vrtArr
            },
            "getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;".func(I, Z, I){ (vt, clsAddr, pub) =>
              
              val cls = vt.ClsTable(imm.Type.Cls(vt.toRealObj[String](vt.obj(clsAddr).apply("name"))))
              vt.alloc(implicit r =>
                rt.Obj.allocArr(imm.Type.Cls("java/lang/reflect/Method"),
                  cls.methods.map{ m =>
                    rt.Obj.alloc(vt.ClsTable(imm.Type.Cls("java/lang/reflect/Method"))).address
                  }
                )
              )()
            },
            "getEnclosingMethod0()[Ljava/lang/Object;".func(I, I){(vt, cls) => 0},
            "getDeclaringClass()Ljava/lang/Class;".func(I, I){ (vt, cls) => 0},
            "getInterfaces()[Ljava/lang/Class;".func(I, I){ (vt, clsAddr) =>
              
              val cls = vt.ClsTable(imm.Type.Cls(vt.toRealObj[String](vt.obj(clsAddr).apply("name"))))
              vt.alloc(implicit r =>
                rt.Obj.allocArr(imm.Type.Cls("java/lang/Class"),
                  cls.typeAncestry
                     .filter(x => !cls.clsAncestry.contains(x))
                     .toSeq
                     .map(x => vt.typeObjCache(vt.ClsTable(x).tpe))
                )
              )
            },
            "getModifiers()I".func(I, I){ (vt, o) =>
              
              val topClsName = vt.toRealObj[String](vt.obj(o).apply("name"))

              vt.ClsTable(imm.Type.Cls(topClsName)).accessFlags
            },
            "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;".func(I, I){ (vt, o) =>
              
              vt.typeObjCache(imm.Type.readJava(vt.toRealObj[String](o)))()
            },
            "getSuperclass()Ljava/lang/Class;".func(I, I){ (vt, o) =>
              
              val topClsName = vt.toRealObj[String](vt.obj(o).apply("name"))

              vt.ClsTable(imm.Type.Cls(topClsName))
                .superType
                .map{_.name}
                .map(name => vt.typeObjCache(imm.Type.readJava(name))())
                .getOrElse(0)


            },

            "isArray()Z".func(I, I){ (vt, o) =>
              
              if(vt.toRealObj[String](vt.obj(o).apply("name")).contains('[')) 1 else 0

            },
            "isAssignableFrom(Ljava/lang/Class;)Z".func(I, I, I){ (vt, a, b) =>
              
              val clsA = vt.obj(a)
              val clsB = vt.obj(b)
              val nameA = vt.toRealObj[String](clsA("name"))
              val nameB = vt.toRealObj[String](clsB("name"))

              def check(s: imm.Type, t: imm.Type): Boolean = {

                (s, t) match{

                  case (s: imm.Type.Cls, t: imm.Type.Cls) => vt.ClsTable(s).typeAncestry.contains(t)
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
              
              val clsObj = vt.obj(o)
              vt.ClsTable(imm.Type.Cls(vt.toRealObj[String](clsObj("name")))).isInterface
            },
            "isPrimitive()Z".func(I, I){ (vt, o) =>
              
              val clsObj = vt.obj(o)
              val name = vt.toRealObj[String](clsObj("name"))
              val res = Prim.allJava.contains(name)
              if (res) 1 else 0
            },
            "registerNatives()V".value(V)(())
          ),
          "ClassLoader"/(
            "getCaller(I)Ljava/lang/Class;".func(I, I){ (vt, o) =>
              
              val name = o match{
                case 0 => "java/lang/ClassLoader"
                case 1 => vt.runningClassName(0)
                case 2 => vt.runningClassName(1)
              }
              vt.typeObjCache(imm.Type.readJava(name))()
            },
            "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;".func(I, I){ (vt, o) =>
              

              val name = vt.toRealObj[String](o)
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
                vt.alloc(vt.toVirtObj(byteStream)(_))
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

              


              val string =
                if(vt.isObj(value)) vt.obj(value).cls.tpe.javaName
                else vt.arr(value).tpe.javaName

              vt.typeObjCache(imm.Type.readJava(string))()
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
              val size = vt.arrayTypeCache(vt.heap(src)).size
              System.arraycopy(
                vt.heap.memory,
                src + (srcIndex * size) + Constants.arrayHeaderSize,
                vt.heap.memory,
                dest + (destIndex * size) + Constants.arrayHeaderSize,
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
              
              val str = vt.toRealObj[String](addr)
              val result = vt.internedStrings.getOrElseUpdate(str, addr)
              result
            }
          ),

          "Thread"/(
            "registerNatives()V".value(V)(()),
            "currentThread()Ljava/lang/Thread;".func(I){ vt =>
              

              vt.currentThread
            },
            "setPriority0(I)V".value(V)(()),
            "isAlive()Z".value(Z)(false),
            "start0()V".value(V)(())
          ),
          "Throwable"/(
            "fillInStackTrace()Ljava/lang/Throwable;".func(I, I){ (vt, addr) =>
              
              val throwable = vt.obj(addr)
              val trace = vt.trace
              throwable("stackTrace") = vt.alloc(vt.toVirtObj(vt.trace)(_))

              throwable.address()
            }
          ),
          "reflect"/(
            "Array"/(
              "newArray(Ljava/lang/Class;I)Ljava/lang/Object;".func(I, I, I){ (vt, cls, length) =>
                
                val clsObj = vt.obj(cls)
                val clsName = vt.toRealObj[String](clsObj("name"))
                vt.alloc(rt.Obj.allocArr(imm.Type.readJava(clsName), length)(_)).address()
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
                (vt, constr, argArr) => vt.newInstance(constr, argArr)

              }
            ),
            "NativeConstructorAccessorImpl"/(
              "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;".func(I, I, I){
                (vt, cons, args) =>
                  
                  val name = vt.toRealObj[String](vt.obj(vt.obj(cons).apply("clazz")).apply("name"))
                  vt.alloc(implicit r =>
                    rt.Obj.alloc(vt.ClsTable(imm.Type.Cls(name)))
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
              vt.invokeRun(a)
            },
            "doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;".func(I, I){ (vt, a) =>
              vt.invokeRun(a)
            },
            "doPrivileged(Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;".func(I, I, I){ (vt, a, b) =>
              vt.invokeRun(a)
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
            

            if (vt.isObj(o)){
              val thing = vt.obj(o)
              println("Virtual\t" + vt.toRealObj[Object](thing.address()))
            }else if(vt.isArr(o)){
              val s = Virtualizer.popVirtual(imm.Type.Arr(vt.arrayTypeCache(vt.heap(o + 1))), () => o)(vt)
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
              val res = vt.offHeapPointer
              vt.setOffHeapPointer(vt.offHeapPointer + size)
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
                vt.offHeap(offset.toInt + i) = bs.get(i)
              }
              ()
            },
            "getByte(J)B".func(I, J, B) { (vt, unsafe, offset) =>
              val res = vt.offHeap(offset.toInt)
              res
            },
            "arrayBaseOffset(Ljava/lang/Class;)I".value(I)(0),
            "arrayIndexScale(Ljava/lang/Class;)I".value(I)(1),
            "allocateInstance(Ljava/lang/Class;)Ljava/lang/Object;".func(I, I, I){ (vt, unsafe, clsPtr) =>
              
              val name = vt.toRealObj[String](vt.obj(clsPtr).apply("name"))
              val x = vt.alloc{ implicit r =>
                rt.Obj.alloc(vt.ClsTable(imm.Type.Cls(name))).address()
              }
              x
            },
            "addressSize()I".value(I)(4),
            "compareAndSwapInt(Ljava/lang/Object;JII)Z".func(I, I, J, I, I, Z){ (vt, unsafe, o, slot, expected ,x) =>
              
              val obj = vt.obj(o)
              if (obj.members(slot.toInt) == expected){
                obj.members(slot.toInt) = x
                true
              }else{
                false
              }

            },
            "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z".func(I, I, J, I, I, Z){ (vt, unsafe, o, slot, expected ,x) =>
              
              val obj = vt.obj(o)
              if (obj.members(slot.toInt) == expected){
                obj.members(slot.toInt) = x
                true
              }else{
                false
              }

            },
            "compareAndSwapLong(Ljava/lang/Object;JJJ)Z".func(I, I, J, J, J, Z){ (vt, unsafe, o, slot, expected ,x) =>
              
              val obj = vt.obj(o)
              val current = J.read(Util.reader(obj.members, slot.toInt))
              if (current == expected){
                J.write(x, Util.writer(obj.members, slot.toInt))
                true
              }else{
                false
              }
            },
            "ensureClassInitialized(Ljava/lang/Class;)V".func(I, I, V){ (vt, unsafe, cls) =>
              ()
            },
            "getObject(Ljava/lang/Object;J)Ljava/lang/Object;".func(I, I, J, I){ (vt, unsafe, o, offset) =>

              vt.obj(o).members(offset.toInt)
            },
            "getBooleanVolatile(Ljava/lang/Object;J)Z".func(I, I, J, Z){ (vt, unsafe, o, offset) =>

              vt.obj(o).members(offset.toInt) != 0
            },
            "putBooleanVolatile(Ljava/lang/Object;JZ)V".func(I, I, J, Z, V){ (vt, unsafe, o, offset, bool) =>
              
              Z.write(bool, vt.obj(o).members(offset.toInt) = _)
            },
            "getByteVolatile(Ljava/lang/Object;J)B".func(I, I, J, B){ (vt, unsafe, o, offset) =>

              vt.obj(o).members(offset.toInt).toByte
            },
            "putByteVolatile(Ljava/lang/Object;JB)V".func(I, I, J, B, V){ (vt, unsafe, o, offset, byte) =>
              
              B.write(byte, vt.obj(o).members(offset.toInt) = _)
            },
            "getCharVolatile(Ljava/lang/Object;J)C".func(I, I, J, C){ (vt, unsafe, o, offset) =>

              vt.obj(o).members(offset.toInt).toChar
            },
            "putCharVolatile(Ljava/lang/Object;JC)V".func(I, I, J, C, V){ (vt, unsafe, o, offset, char) =>
              
              C.write(char, vt.obj(o).members(offset.toInt) = _)
            },
            "getInt(Ljava/lang/Object;J)I".func(I, I, J, I){ (vt, unsafe, o, offset) =>

              vt.obj(o).members(offset.toInt)
            },
            "getIntVolatile(Ljava/lang/Object;J)I".func(I, I, J, I){ (vt, unsafe, o, offset) =>

              vt.obj(o).members(offset.toInt)
            },
            "putInt(Ljava/lang/Object;JI)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, int) =>
              
              I.write(int, vt.obj(o).members(offset.toInt) = _)
            },
            "getFloat(Ljava/lang/Object;J)F".func(I, I, J, F){ (vt, unsafe, o, offset) =>
              
              F.read(() => vt.obj(o).members(offset.toInt))
            },
            "putFloat(Ljava/lang/Object;JF)V".func(I, I, J, F, V){ (vt, unsafe, o, offset, float) =>
              
              F.write(float, vt.obj(o).members(offset.toInt) = _)
            },
            "getLongVolatile(Ljava/lang/Object;J)J".func(I, I, J, J){ (vt, unsafe, o, offset) =>
              
              J.read(Util.reader(vt.obj(o).members, offset.toInt))
            },
            "putLongVolatile(Ljava/lang/Object;JJ)V".func(I, I, J, J, V){ (vt, unsafe, o, offset, long) =>
              
              J.write(long, Util.writer(vt.obj(o).members, offset.toInt))
            },
            "getDouble(Ljava/lang/Object;J)D".func(I, I, J, D){ (vt, unsafe, o, offset) =>
              
              D.read(Util.reader(vt.obj(o).members, offset.toInt))
            },
            "putDouble(Ljava/lang/Object;JD)V".func(I, I, J, D, V){ (vt, unsafe, o, offset, double) =>
              
              D.write(double, Util.writer(vt.obj(o).members, offset.toInt))
            },
            "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;".func(I, I, J, I){ (vt, unsafe, o, offset) =>

              vt.obj(o).members(offset.toInt)
            },
            "putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, ref) =>

              vt.obj(o).members(offset.toInt) = ref
            },
            "putObject(Ljava/lang/Object;JLjava/lang/Object;)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, ref) =>

              vt.obj(o).members(offset.toInt) = ref
            },
            "putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V".func(I, I, J, I, V){ (vt, unsafe, o, offset, ref) =>
              
              if (vt.isObj(o))
                vt.obj(o).members(offset.toInt) = ref
              else{
                vt.arr(o)(offset.toInt) = ref
              }
            },
            "objectFieldOffset(Ljava/lang/reflect/Field;)J".func(I, I, J){(vt, unsafe, f) =>
              
              val field = vt.obj(f)
              val s = field.apply("slot")
              s
            },
            "staticFieldOffset(Ljava/lang/reflect/Field;)J".func(I, I, J){ (vt, unsafe, f) =>
              
              val field = vt.obj(f)
              field.apply("slot")
            },
            "staticFieldBase(Ljava/lang/reflect/Field;)Ljava/lang/Object;".func(I, I, I){(vt, unsafe, f) =>
              
              vt.ClsTable(imm.Type.Cls(vt.toRealObj[String](vt.obj(vt.obj(f).apply("clazz")).apply("name")))).statics()
            },
            "registerNatives()V".value(V)(()),
            "getUnsafe()Lsun/misc/Unsafe;".func(I){vt => vt.theUnsafe.address()},
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
              
              if (n >= vt.threadStackLength) 0
              else {
                val name = vt.runningClassName(n)
                vt.typeObjCache(imm.Type.readJava(name))()
              }
            },
            "getCallerClass()Ljava/lang/Class;".func(I){ (vt) =>
              
              val n = 1
              if (n >= vt.threadStackLength) 0
              else {
                val name = vt.runningClassName(n)
                vt.typeObjCache(imm.Type.readJava(name))()
              }
            },
            "getClassAccessFlags(Ljava/lang/Class;)I".func(I, I){ (vt, o) =>
              
              val addr = vt.obj(o).apply("name")
              val str = vt.toRealObj[String](addr)
              vt.ClsTable(imm.Type.Cls(str)).accessFlags
            }
          )
        )
      ),
      "metascala"/(
        "Virtualizer$"/(
            "unsafe()Lsun/misc/Unsafe;".func(I){vt =>
              vt.theUnsafe.address()
            }
        )
      )
    ).toRoute()
  }
}