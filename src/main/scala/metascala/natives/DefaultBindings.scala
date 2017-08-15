package metascala
package natives


import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.ByteBuffer

import imm.Type.Prim._
import metascala.imm.Type.Prim
import metascala.rt.NativeMethod
import metascala.util._

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

  case class native(clsName: String, sigStr: String) {
    val (methodName, desc) = sigStr.splitAt(sigStr.indexOf('('))
    val sig = imm.Sig(methodName, imm.Desc.read(desc))
    def apply[T: Prim](f: (Bindings.Interface, () => Int) => T) = {
      NativeMethod(
        clsName, sig,
        (vt, arg, ret) => implicitly[Prim[T]].write(f(vt, arg), ret)
      )
    }
    def func[T](out: Prim[T])(f: Bindings.Interface => T) =
      NativeMethod(clsName, sig,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t), ret)
      )
    def func[A, T](a: Prim[A], out: Prim[T])(f: (Bindings.Interface, A) => T) =
      NativeMethod(clsName, sig,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args)), ret)
      )
    def func[A, B, T](a: Prim[A], b: Prim[B], out: Prim[T])(f: (Bindings.Interface, A, B) => T) =
      NativeMethod(clsName, sig,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args)), ret)
      )
    def func[A, B, C, T](a: Prim[A], b: Prim[B], c: Prim[C], out: Prim[T])(f: (Bindings.Interface, A, B, C) => T) =
      NativeMethod(clsName, sig,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args), c.read(args)), ret)
      )
    def func[A, B, C, D, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], out: Prim[T])(f: (Bindings.Interface, A, B, C, D) => T) =
      NativeMethod(clsName, sig,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args), c.read(args), d.read(args)), ret)
      )
    def func[A, B, C, D, E, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], e: Prim[E], out: Prim[T])(f: (Bindings.Interface, A, B, C, D, E) => T) =
      NativeMethod(clsName, sig,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args), c.read(args), d.read(args), e.read(args)), ret)
      )

    def func[A, B, C, D, E, F, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], e: Prim[E], f: Prim[F], out: Prim[T])(func: (Bindings.Interface, A, B, C, D, E, F) => T) =
      NativeMethod(clsName, sig,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(func(t, a.read(args), b.read(args), c.read(args), d.read(args), e.read(args), f.read(args)), ret)
      )
    def value[T](out: Prim[T])(x: => T) = func(out)(t => x)
  }
  val trapped = Agg(
    native("java/io/UnixFileSystem", "initIDs()V;") { (vt, arg) => },
    native("java/lang/Class", "getProtectionDomain0()Ljava/security/ProtectionDomain;") { (vt, arg) => },
    native("java/lang/Class", "desiredAssertionStatus0(Ljava/lang/Class;)Z") { (vt, arg) => },
    native("java/lang/Class", "desiredAssertionStatus()Z") { (vt, arg) => },
    native("java/lang/Class", "forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;") { (vt, arg) =>
      val nameString = vt.toRealObj[String](arg())
      val tpe = imm.Type.readJava(nameString)
      try{
        if (!nameString.contains("["))vt.ClsTable(tpe.asInstanceOf[imm.Type.Cls])
        val x = vt.typeObjCache(tpe)()
        x
      } catch{case e: Exception =>
        vt.throwExWithTrace("java/lang/ClassNotFoundException", nameString)
        0
      }
    },
    native("java/lang/Class", "getClassLoader0()Ljava/lang/ClassLoader;") { (vt, arg) => },
    native("java/lang/Class", "getComponentType()Ljava/lang/Class;") { (vt, arg) =>
      val obj = vt.obj(arg())

      val oldName = vt.toRealObj[String](obj("name"))
      vt.typeObjCache(imm.Type.Arr.readJava(oldName).innerType)()
    },
    native("java/lang/Class", "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;"){(vt, arg) =>
      val obj = vt.obj(arg())

      val name = vt.toRealObj[String](obj("name"))
      val cls = vt.ClsTable(name)
      val realFields = cls.fieldList ++ cls.staticList

      vt.alloc(implicit r =>
        r.newArr("java/lang/reflect/Field",
          realFields.zipWithIndex.map{ case (f, i) =>
            r.newObj("java/lang/reflect/Field",
              "clazz" -> obj.address,
              "slot" -> Ref.Raw((if (f.static) cls.staticList else cls.fieldList).indexOf(f)),
              "name" -> vt.internedStrings.getOrElseUpdate(f.name, vt.toVirtObj(f.name)),
              "modifiers" -> Ref.Raw(f.access),
              "type" -> vt.typeObjCache(f.desc)
            ).address
          }
        )
      ).address()
      // if (f.static) cls.staticList else cls.fieldList).indexOf(f)
      // f.static(cls.staticList, cls.fieldList).indexOf(f)
    },
    native("java/lang/Class", "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;"){ (vt, arg) =>

      val clsObj = vt.obj(arg())
      val clsName = vt.toRealObj[String](clsObj("name"))
      val cls = vt.ClsTable(clsName)
      val realMethods = cls.methods.filter(_.sig.name == "<init>")
      val vrtArr = vt.alloc(implicit r =>
        r.newArr("java/lang/reflect/Constructor",
          realMethods.zipWithIndex.map{ case (f, i) =>
            r.newObj("java/lang/reflect/Constructor",
              "clazz" -> clsObj.address,
              "slot" -> Ref.Raw(i),
              "signature" -> vt.toVirtObj(f.sig.desc.unparse),
              "parameterTypes" -> r.newArr("java/lang/Class",
                f.sig.desc.args.map(t =>
                  vt.typeObjCache(imm.Type.readJava(t.realCls.getName))
                )
              ),
              "modifiers" -> Ref.Raw(f.accessFlags)
            ).address
          }
        )
      )
      vrtArr.address()
    },
    native("java/nio/charset/Charset", "defaultCharset()Ljava/nio/charset/Charset;"){(vt, arg) =>
      vt.invoke(
        "metascala/DummyCharset",
        imm.Sig("getValue", imm.Desc.read("()Ljava/nio/charset/Charset;")),
        Agg.empty
      )

      vt.returnedVal(0)
    },
    native("java/lang/Class", "getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;"){ (vt, arg) =>

      val cls = vt.ClsTable(vt.toRealObj[String](vt.obj(arg()).apply("name")))
      vt.alloc(implicit r =>
        r.newArr("java/lang/reflect/Method",
          cls.methods.map{ m =>
            r.newObj("java/lang/reflect/Method",
              "name" -> vt.internedStrings.getOrElseUpdate(
                m.sig.name, vt.toVirtObj(m.sig.name).address
              )
              ,
              "returnType" -> vt.typeObjCache(m.sig.desc.ret),
              "parameterTypes" -> r.newArr("java/lang/Class",
                m.sig.desc.args.map(vt.typeObjCache)
              )
            ).address
          }
        )
      )()
    },
    native("java/lang/Class", "getEnclosingMethod0()[Ljava/lang/Object;"){(vt, arg) => 0},
    native("java/lang/Class", "getDeclaringClass()Ljava/lang/Class;"){(vt, arg) => 0},
    native("java/lang/Class", "getInterfaces()[Ljava/lang/Class;"){(vt, arg) =>

      val cls = vt.ClsTable(vt.toRealObj[String](vt.obj(arg()).apply("name")))
      vt.alloc(implicit r =>
        r.newArr("java/lang/Class",
          cls.typeAncestry
            .filter(x => !cls.clsAncestry.contains(x))
            .toSeq
            .map(x => vt.typeObjCache(vt.ClsTable(x).tpe))
        )
      ).address()
    },
    native("java/lang/Class", "getModifiers()I"){(vt, arg) =>

      val topClsName = vt.toRealObj[String](vt.obj(arg()).apply("name"))

      vt.ClsTable(topClsName).accessFlags
    },
    native("java/lang/Class", "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"){(vt, arg) =>

      vt.typeObjCache(imm.Type.readJava(vt.toRealObj[String](arg())))()
    },
    native("java/lang/Class", "getSuperclass()Ljava/lang/Class;"){(vt, arg) =>

      val topClsName = vt.toRealObj[String](vt.obj(arg()).apply("name"))

      vt.ClsTable(topClsName)
        .superType
        .map{_.name}
        .map(name => vt.typeObjCache(imm.Type.readJava(name))())
        .getOrElse(0)


    },

    native("java/lang/Class", "isArray()Z"){(vt, arg) =>

      if(vt.toRealObj[String](vt.obj(arg()).apply("name")).contains('[')) 1 else 0

    },
    native("java/lang/Class", "isAssignableFrom(Ljava/lang/Class;)Z"){ (vt, arg) =>

      val clsA = vt.obj(arg())
      val clsB = vt.obj(arg())
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
    native("java/lang/Class", "isInterface()Z"){(vt, arg) =>

      val clsObj = vt.obj(arg())
      vt.ClsTable(vt.toRealObj[String](clsObj("name"))).isInterface
    },
    native("java/lang/Class", "isPrimitive()Z"){(vt, arg) =>

      val clsObj = vt.obj(arg())
      val name = vt.toRealObj[String](clsObj("name"))
      val res = Prim.allJava.contains(name)
      if (res) 1 else 0
    },
    native("java/lang/Class", "registerNatives()V"){(vt, arg) => },
    native("java/lang/ClassLoader", "getCaller(I)Ljava/lang/Class;"){ (vt, arg) =>

      val name = arg() match{
        case 0 => "java/lang/ClassLoader"
        case 1 => vt.runningClassName(0)
        case 2 => vt.runningClassName(1)
      }
      vt.typeObjCache(imm.Type.readJava(name))()
    },
    native("java/lang/ClassLoader", "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;"){ (vt, arg) =>


      val name = vt.toRealObj[String](arg())
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
        vt.alloc(vt.toVirtObj(byteStream)(_)).address()
      }
    },
    native("java/lang/ClassLoader", "registerNatives()V"){(vt, arg) =>},
    native("java/lang/Double", "doubleToRawLongBits(D)J"){(vt, arg) => imm.Type.Prim.J.read(arg)},
    native("java/lang/Double", "longBitsToDouble(J)D"){(vt, arg) => imm.Type.Prim.J.read(arg)},
    native("java/lang/Float", "intBitsToFloat(I)F"){(vt, arg) => arg()},
    native("java/lang/Float", "floatToRawIntBits(F)I"){(vt, arg) => arg()},
    native("java/lang/Object", "clone()Ljava/lang/Object;"){(vt, arg) => arg()},
    native("java/lang/Object", "getClass()Ljava/lang/Class;"){ (vt, arg) =>
      val value = arg()
      val string =
        if(vt.isObj(value)) vt.obj(value).cls.tpe.javaName
        else vt.arr(value).tpe.javaName

      vt.typeObjCache(imm.Type.readJava(string))()
    },

    native("java/lang/Object", "hashCode()I"){(vt, arg) => arg()},
    native("java/lang/Object", "registerNatives()V"){(vt, arg) => },
    native("java/lang/Runtime", "freeMemory()J"){(vt, arg) => 4*1024*1024},
    native("java/lang/Runtime", "availableProcessors()I"){(vt, arg) => 1},
    native("java/lang/System", "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"){ (vt, arg) =>
      val (src, srcIndex, dest, destIndex, length) = (arg(), arg(), arg(), arg(), arg())
      val size = vt.arr(src).innerType.size
      System.arraycopy(
        vt.heap.memory,
        src + (srcIndex * size) + Constants.arrayHeaderSize,
        vt.heap.memory,
        dest + (destIndex * size) + Constants.arrayHeaderSize,
        length * size
      )
    },

    native("java/lang/System", "identityHashCode(Ljava/lang/Object;)I"){(vt, arg) => arg()},
    native("java/lang/System", "nanoTime()J"){(vt, arg) => System.nanoTime()},
    native("java/lang/System", "currentTimeMillis()J"){(vt, arg) =>System.currentTimeMillis()},
    native("java/lang/System", "getProperty(Ljava/lang/String;)Ljava/lang/String;"){(vt, arg) => 0},
    native("java/lang/System", "getProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;"){(vt, arg) => 0},
    native("java/lang/System", "registerNatives()V"){(vt, arg) =>},
    native("java/lang/String", "intern()Ljava/lang/String;"){ (vt, arg) =>
      val addr = arg()
      val str = vt.toRealObj[String](addr)
      val result = vt.internedStrings.getOrElseUpdate(str, new Ref.UnsafeManual(addr))
      result()
    },
    native("java/lang/Thread", "registerNatives()V"){(vt, arg) => ()},
    native("java/lang/Thread", "currentThread()Ljava/lang/Thread;"){(vt, arg) => vt.currentThread},
    native("java/lang/Thread", "setPriority0(I)V"){(vt, arg) => ()},
    native("java/lang/Thread", "isAlive()Z"){(vt, arg) => false},
    native("java/lang/Thread", "start0()V"){(vt, arg) => ()},
    native("java/lang/Throwable", "fillInStackTrace()Ljava/lang/Throwable;"){ (vt, arg) =>
      val throwable = vt.obj(arg())
      val trace = vt.trace
      throwable("stackTrace") = vt.alloc(vt.toVirtObj(vt.trace)(_))
      throwable.address()
    },
    native("java/lang/reflect/Array", "newArray(Ljava/lang/Class;I)Ljava/lang/Object;"){ (vt, arg) =>
      val (cls, length) = (arg(), arg())

      val clsObj = vt.obj(cls)
      val clsName = vt.toRealObj[String](clsObj("name"))
      vt.alloc(_.newArr(imm.Type.readJava(clsName), length)).address()
    },
    native("java/lang/reflect/Array", "set(Ljava/lang/Object;ILjava/lang/Object;)V"){ (vt, arg) =>
      val (arr, index, obj) = (arg(), arg(), arg())
      vt.invoke(
        "metascala/patches/java/lang/reflect/Array",
        imm.Sig("set", imm.Desc.read("(Ljava/lang/Object;ILjava/lang/Object;)V")),
        Agg(arr, index, obj)
      )
    },
    native("java/lang/reflect/Constructor", "newInstance([Ljava/lang/Object;)Ljava/lang/Object;"){
      (vt, arg) => vt.newInstance(arg(), arg())
    },
    native("java/lang/reflect/NativeConstructorAccessorImpl", "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;"){
      (vt, arg) =>
        val (cons, args) = (arg(), arg())

        val name = vt.toRealObj[String](vt.obj(vt.obj(cons).apply("clazz")).apply("name"))
        vt.alloc(_.newObj(name)).address()
    },
    native("java/lang/StrictMath", "log(D)D").func(D, D){ (vt, arg) =>
      math.log(arg)
    },
    native("java/lang/StrictMath", "pow(DD)D").func(D, D, D) { (vt, arg1, arg2) =>
      math.pow(arg1, arg2)
    },
    native("java/security/AccessController", "doPrivileged(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;"){ (vt, arg) =>
      vt.invokeRun(arg())
    },
    native("java/security/AccessController", "doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;"){ (vt, arg) =>
      vt.invokeRun(arg())
    },
    native("java/security/AccessController", "doPrivileged(Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;"){ (vt, arg) =>
      vt.invokeRun(arg())
    },
    native("java/security/AccessController", "getStackAccessControlContext()Ljava/security/AccessControlContext;"){(vt, arg) => 0},
    native("java/util/concurrent/atomic/AtomicLong", "VMSupportsCS8()Z").value(Z)(true),
    native("java/util/TimeZone", "getSystemTimeZoneID(Ljava/lang/String;)java/lang/String").value(I)(0),
    native("java/util/TimeZone", "getSystemGMTOffsetID()java/lang/String").func(I){ vt =>
      vt.alloc(r => vt.toVirtObj("GMT+08:00")(r)).address()
    },
    native("scala/Predef$", "println(Ljava/lang/Object;)V").func(I, I, V){ (vt, predef, o) =>
      if (vt.isObj(o)){
        val thing = vt.obj(o)
        println("Virtual\t" + vt.toRealObj[Object](thing.address()))
      }else if(vt.isArr(o)){
        val s = Virtualizer.popVirtual(
          vt.arr(o + 1).tpe, () => o
        )(vt)
        println("Virtual\t" + s)
      }else{
        println("Virtual\t" + null)
      }
    },
    native("sun/misc/Hashing", "randomHashSeed(Ljava/lang/Object;)I").value(I)(31337), // sufficiently random
    native("sun/misc/Unsafe", "allocateMemory(J)J").func(I, J, J){ (vt, unsafe, size) =>
      val res = vt.offHeapPointer
      vt.setOffHeapPointer(vt.offHeapPointer + size)
      res
    },
    native("sun/misc/Unsafe", "freeMemory(J)V").value(V)(()),// Do nothing lol
    native("sun/misc/Unsafe", "putLong(JJ)V").func(I, J, J, V){ (vt, unsafe, offset, value) =>
      val bs = ByteBuffer.allocate(8)
      bs.putLong(value)

      for(i <- 0 until 8) {
        vt.offHeap(offset.toInt + i) = bs.get(i)
      }
      ()
    },
    native("sun/misc/Unsafe", "getByte(J)B").func(I, J, B){ (vt, unsafe, offset) =>
      val res = vt.offHeap(offset.toInt)
      res
    },
    native("sun/misc/Unsafe", "arrayBaseOffset(Ljava/lang/Class;)I").value(I)(0),
    native("sun/misc/Unsafe", "arrayIndexScale(Ljava/lang/Class;)I").value(I)(1),
    native("sun/misc/Unsafe", "allocateInstance(Ljava/lang/Class;)Ljava/lang/Object;").func(I, I, I){ (vt, unsafe, clsPtr) =>

      val name = vt.toRealObj[String](vt.obj(clsPtr).apply("name"))
      vt.alloc(_.newObj(name)).address()
    },
    native("sun/misc/Unsafe", "addressSize()I").value(I)(4),
    native("sun/misc/Unsafe", "compareAndSwapInt(Ljava/lang/Object;JII)Z").func(I, I, J, I, I, Z){
      (vt, unsafe, o, slot, expected, x) =>

        val obj = vt.obj(o)
        if (obj.members(slot.toInt) == expected){
          obj.members(slot.toInt) = x
          true
        }else{
          false
        }
    },
    native("sun/misc/Unsafe", "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z").func(I, I, J, I, I, Z){
      (vt, unsafe, o, slot, expected, x) =>

        val obj = vt.obj(o)
        if (obj.members(slot.toInt) == expected){
          obj.members(slot.toInt) = x
          true
        }else{
          false
        }

    },
    native("sun/misc/Unsafe", "compareAndSwapLong(Ljava/lang/Object;JJJ)Z").func(I, I, J, J, J, Z){ (vt, unsafe, o, slot, expected, x) =>
      val obj = vt.obj(o)
      val current = J.read(Util.reader(obj.members, slot.toInt))
      if (current == expected){
        J.write(x, Util.writer(obj.members, slot.toInt))
        true
      }else{
        false
      }
    },
    native("sun/misc/Unsafe", "ensureClassInitialized(Ljava/lang/Class;)V").value(V)(()),
    native("sun/misc/Unsafe", "getObject(Ljava/lang/Object;J)Ljava/lang/Object;").func(I, I, J, I){
      (vt, unsafe, o, offset) => vt.obj(o).members(offset.toInt)
    },
    native("sun/misc/Unsafe", "getBooleanVolatile(Ljava/lang/Object;J)Z").func(I, I, J, Z){
      (vt, unsafe, o, offset) => vt.obj(o).members(offset.toInt) != 0
    },
    native("sun/misc/Unsafe", "putBooleanVolatile(Ljava/lang/Object;JZ)V").func(I, I, J, Z, V){
      (vt, unsafe, o, offset, bool) => Z.write(bool, vt.obj(o).members(offset.toInt) = _)
    },
    native("sun/misc/Unsafe", "getByteVolatile(Ljava/lang/Object;J)B").func(I, I, J, B){
      (vt, unsafe, o, offset) => vt.obj(o).members(offset.toInt).toByte
    },
    native("sun/misc/Unsafe", "putByteVolatile(Ljava/lang/Object;JB)V").func(I, I, J, B, V){
      (vt, unsafe, o, offset, byte) => B.write(byte, vt.obj(o).members(offset.toInt) = _)
    },
    native("sun/misc/Unsafe", "getCharVolatile(Ljava/lang/Object;J)C").func(I, I, J, C){
      (vt, unsafe, o, offset) => vt.obj(o).members(offset.toInt).toChar
    },
    native("sun/misc/Unsafe", "putCharVolatile(Ljava/lang/Object;JC)V").func(I, I, J, C, V){
      (vt, unsafe, o, offset, char) => C.write(char, vt.obj(o).members(offset.toInt) = _)
    },
    native("sun/misc/Unsafe", "getInt(Ljava/lang/Object;J)I").func(I, I, J, I){
      (vt, unsafe, o, offset) => vt.obj(o).members(offset.toInt)
    },
    native("sun/misc/Unsafe", "getIntVolatile(Ljava/lang/Object;J)I").func(I, I, J, I){
      (vt, unsafe, o, offset) => vt.obj(o).members(offset.toInt)
    },
    native("sun/misc/Unsafe", "putInt(Ljava/lang/Object;JI)V").func(I, I, J, I, V){
      (vt, unsafe, o, offset, int) => I.write(int, vt.obj(o).members(offset.toInt) = _)
    },
    native("sun/misc/Unsafe", "getFloat(Ljava/lang/Object;J)F").func(I, I, J, F){
      (vt, unsafe, o, offset) => F.read(() => vt.obj(o).members(offset.toInt))
    },
    native("sun/misc/Unsafe", "putFloat(Ljava/lang/Object;JF)V").func(I, I, J, F, V){
      (vt, unsafe, o, offset, float) => F.write(float, vt.obj(o).members(offset.toInt) = _)
    },
    native("sun/misc/Unsafe", "getLongVolatile(Ljava/lang/Object;J)J").func(I, I, J, J){
      (vt, unsafe, o, offset) => J.read(Util.reader(vt.obj(o).members, offset.toInt))
    },
    native("sun/misc/Unsafe", "putLongVolatile(Ljava/lang/Object;JJ)V").func(I, I, J, J, V){
      (vt, unsafe, o, offset, long) => J.write(long, Util.writer(vt.obj(o).members, offset.toInt))
    },
    native("sun/misc/Unsafe", "getDouble(Ljava/lang/Object;J)D").func(I, I, J, D){
      (vt, unsafe, o, offset) => D.read(Util.reader(vt.obj(o).members, offset.toInt))
    },
    native("sun/misc/Unsafe", "putDouble(Ljava/lang/Object;JD)V").func(I, I, J, D, V){
      (vt, unsafe, o, offset, double) => D.write(double, Util.writer(vt.obj(o).members, offset.toInt))
    },
    native("sun/misc/Unsafe", "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;").func(I, I, J, I){
      (vt, unsafe, o, offset) => vt.obj(o).members(offset.toInt)
    },
    native("sun/misc/Unsafe", "putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)V").func(I, I, J, I, V){
      (vt, unsafe, o, offset, ref) => vt.obj(o).members(offset.toInt) = ref
    },
    native("sun/misc/Unsafe", "putObject(Ljava/lang/Object;JLjava/lang/Object;)V").func(I, I, J, I, V){
      (vt, unsafe, o, offset, ref) => vt.obj(o).members(offset.toInt) = ref
    },
    native("sun/misc/Unsafe", "putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V").func(I, I, J, I, V){
      (vt, unsafe, o, offset, ref) =>
        if (vt.isObj(o)) vt.obj(o).members(offset.toInt) = ref
        else vt.arr(o)(offset.toInt) = ref
    },
    native("sun/misc/Unsafe", "objectFieldOffset(Ljava/lang/reflect/Field;)J").func(I, I, J){
      (vt, unsafe, f) => vt.obj(f).apply("slot")
    },
    native("sun/misc/Unsafe", "staticFieldOffset(Ljava/lang/reflect/Field;)J").func(I, I, J){
      (vt, unsafe, f) =>vt.obj(f).apply("slot")
    },
    native("sun/misc/Unsafe", "staticFieldBase(Ljava/lang/reflect/Field;)Ljava/lang/Object;").func(I, I, I){
      (vt, unsafe, f) => vt.ClsTable(vt.toRealObj[String](vt.obj(vt.obj(f).apply("clazz")).apply("name"))).statics()
    },
    native("sun/misc/Unsafe", "registerNatives()V").value(V)(()),
    native("sun/misc/Unsafe", "getUnsafe()Lsun/misc/Unsafe;").func(I){vt => vt.theUnsafe.address()},
    native("sun/misc/Unsafe", "<clinit>()V").value(V)(()),
    native("sun/misc/VM", "getSavedProperty(Ljava/lang/String;)Ljava/lang/String;").value(I)(0),
    native("sun/misc/VM", "initialize()V").value(V)(()),
    native("sun/reflect/Reflection", "filterFields(Ljava/lang/Class;[Ljava/lang/reflect/Field;)[Ljava/lang/reflect/Field;").func(I, I, I){ (vt, cls, fs) =>
      fs
    },
    native("sun/reflect/Reflection", "getCallerClass(I)Ljava/lang/Class;").func(I, I){ (vt, n) =>

      if (n >= vt.threadStackLength) 0
      else {
        val name = vt.runningClassName(n)
        vt.typeObjCache(imm.Type.readJava(name))()
      }
    },
    native("sun/reflect/Reflection", "getCallerClass()Ljava/lang/Class;").func(I){ (vt) =>

      val n = 1
      if (n >= vt.threadStackLength) 0
      else {
        val name = vt.runningClassName(n)
        vt.typeObjCache(imm.Type.readJava(name))()
      }
    },
    native("sun/reflect/Reflection", "getClassAccessFlags(Ljava/lang/Class;)I").func(I, I){ (vt, o) =>

      val addr = vt.obj(o).apply("name")
      val str = vt.toRealObj[String](addr)
      val res = vt.ClsTable(str).accessFlags
      println("getClassAccessFlags " + str)
      println("getClassAccessFlags " + res)
      res
    },
    native("metascala/Virtualizer$", "unsafe()Lsun/misc/Unsafe;").func(I){vt =>
      vt.theUnsafe.address()
    }
  )
}
