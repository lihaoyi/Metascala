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

  def native(clsName: String, sigStr: String) = {
    Binding(imm.Type.Cls(clsName), imm.Sig.read(sigStr), false)
  }
  case class Binding(cls: imm.Type.Cls, sig: imm.Sig, isStatic: Boolean) {
    def static = this.copy(isStatic = true)

    def apply[T: Prim](f: (Bindings.Interface, () => Int) => T) = {
      NativeMethod(
        cls, sig, isStatic, 0,
        (vt, arg, ret) => implicitly[Prim[T]].write(f(vt, arg), ret)
      )
    }
    def func[T](out: Prim[T])(f: Bindings.Interface => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t), ret)
      )
    def func[A, T](a: Prim[A], out: Prim[T])(f: (Bindings.Interface, A) => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args)), ret)
      )
    def func[A, B, T](a: Prim[A], b: Prim[B], out: Prim[T])(f: (Bindings.Interface, A, B) => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args)), ret)
      )
    def func[A, B, C, T](a: Prim[A], b: Prim[B], c: Prim[C], out: Prim[T])(f: (Bindings.Interface, A, B, C) => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args), c.read(args)), ret)
      )
    def func[A, B, C, D, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], out: Prim[T])(f: (Bindings.Interface, A, B, C, D) => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args), c.read(args), d.read(args)), ret)
      )
    def func[A, B, C, D, E, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], e: Prim[E], out: Prim[T])(f: (Bindings.Interface, A, B, C, D, E) => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(f(t, a.read(args), b.read(args), c.read(args), d.read(args), e.read(args)), ret)
      )

    def func[A, B, C, D, E, F, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], e: Prim[E], f: Prim[F], out: Prim[T])(func: (Bindings.Interface, A, B, C, D, E, F) => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(func(t, a.read(args), b.read(args), c.read(args), d.read(args), e.read(args), f.read(args)), ret)
      )

    def func[A, B, C, D, E, F, G, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], e: Prim[E], f: Prim[F], g: Prim[G], out: Prim[T])(func: (Bindings.Interface, A, B, C, D, E, F, G) => T) =
      NativeMethod(cls, sig, isStatic, 0,
        (t: Bindings.Interface, args: () => Int, ret: Int => Unit) =>
          out.write(func(t, a.read(args), b.read(args), c.read(args), d.read(args), e.read(args), f.read(args), g.read(args)), ret)
      )
    def value[T](out: Prim[T])(x: => T) = func(out)(t => x)
  }
  val trapped = Agg(
    native("java.io.UnixFileSystem", "initIDs()V;").static { (vt, arg) => },
    native("java.lang.Class", "getProtectionDomain0()Ljava/security/ProtectionDomain;") { (vt, arg) => },
    native("java.lang.Class", "isInstance(Ljava/lang/Object;)Z").func(I, I, Z) { (vt, cls0, obj0) =>
      val tpe = vt.getTypeForTypeObj(cls0)
      if (vt.isArr(obj0)) {
        vt.check(vt.arr(obj0).tpe, tpe)
      }else if (vt.isObj(obj0)){
        vt.check(vt.obj(obj0).tpe, tpe)
      }else /*obj is null*/{
        false
      }
    },
    native("java.lang.Class", "desiredAssertionStatus0(Ljava/lang/Class;)Z").static { (vt, arg) => 0 },
    native("java.lang.Class", "desiredAssertionStatus()Z") { (vt, arg) => 0 },
    native("java.lang.Class", "forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;").static  { (vt, arg) =>

      val nameString = vt.readAnyRef[String](arg())
      val tpe = imm.Type.readJava(nameString)
      try{
        if (!nameString.contains("["))vt.clsTable(tpe.asInstanceOf[imm.Type.Cls])
        val x = vt.typeObjCache(tpe)()
        x
      } catch{case e: Exception =>
        vt.throwExWithTrace("java.lang.ClassNotFoundException", nameString)
        0
      }
    },
    native("java.lang.Class", "getClassLoader0()Ljava/lang/ClassLoader;") { (vt, arg) => },
    native("java.lang.Class", "getComponentType()Ljava/lang/Class;") { (vt, arg) =>
      val obj = vt.obj(arg())

      val oldName = vt.readAnyRef[String](obj("name"))
      vt.typeObjCache(imm.Type.Arr.readJava(oldName).innerType)()
    },
    native("java.lang.Class", "getDeclaredFields0(Z)[Ljava/lang/reflect/Field;"){(vt, arg) =>
      val obj = vt.obj(arg())
      val publicOnly = arg() > 0

      val name = vt.readAnyRef[String](obj("name"))
      val cls = vt.clsTable(name)
      val realFields =
        cls.fieldInfo.slottedList.takeRight(cls.fieldList0.length) ++
        cls.staticInfo.slottedList

      vt.alloc(implicit r =>
        r.newArr("java.lang.reflect.Field",
          for((f, i) <- realFields if !publicOnly || (f.access & MHConstants.ACC_PUBLIC) > 0) yield {
            r.newObj("java.lang.reflect.Field",
              "clazz" -> obj.address,
              "slot" -> Ref.Raw(i),
              "name" -> vt.internedStrings.getOrElseUpdate(f.name, vt.toVirtObj(f.name)),
              "modifiers" -> Ref.Raw(f.access & java.lang.reflect.Modifier.fieldModifiers()),
              "type" -> vt.typeObjCache(f.desc)
            ).address
          }
        )
      ).address()
      // if (f.static) cls.staticList else cls.fieldList).indexOf(f)
      // f.static(cls.staticList, cls.fieldList).indexOf(f)
    },
    native("java.lang.Class", "getDeclaredClasses0()[Ljava/lang/Class;"){(vt, arg) =>
      val obj = arg()

      val tpe = vt.getTypeForTypeObj(obj)
      vt.alloc{implicit r =>
        r.newArr("java.lang.Class",
          vt.clsTable(tpe.asInstanceOf[imm.Type.Cls]).innerClasses.map(vt.typeObjCache)
        )
      }.address()
    },
    native("java.lang.Class", "getDeclaredConstructors0(Z)[Ljava/lang/reflect/Constructor;"){ (vt, arg) =>

      val clsObj = vt.obj(arg())
      val publicOnly = arg() > 0
      val clsName = vt.readAnyRef[String](clsObj("name"))
      val cls = vt.clsTable(clsName)
      val realMethods = cls.methods.filter(_.sig.name == "<init>")
      val vrtArr = vt.alloc(implicit r =>
        r.newArr("java.lang.reflect.Constructor",
          for{
            (f, i) <- realMethods.zipWithIndex
            if !publicOnly || (f.accessFlags & MHConstants.ACC_PUBLIC) > 0
          } yield {
            r.newObj("java.lang.reflect.Constructor",
              "clazz" -> clsObj.address,
              "slot" -> Ref.Raw(i),
              "signature" -> vt.toVirtObj(f.sig.desc.unparse),
              "parameterTypes" -> r.newArr("java.lang.Class",
                f.sig.desc.args.map(t =>
                  vt.typeObjCache(imm.Type.readJava(t.realCls.getName))
                )
              ),
              "modifiers" -> Ref.Raw(f.accessFlags & java.lang.reflect.Modifier.classModifiers())
            ).address
          }
        )
      )
      vrtArr.address()
    },
    native("java.lang.Class", "getDeclaredMethods0(Z)[Ljava/lang/reflect/Method;"){ (vt, arg) =>

      val cls = vt.clsTable(
        vt.readAnyRef[String](vt.obj(arg()).apply("name"))
      )
      val publicOnly = arg() > 0
      vt.alloc(implicit r =>
        r.newArr("java.lang.reflect.Method",
          for{
            m <- cls.methods
            if m.sig.name != "<init>" && m.sig.name != "<clinit>" &&
               (!publicOnly || (m.accessFlags & MHConstants.ACC_PUBLIC) > 0)
          } yield {
            r.newObj("java.lang.reflect.Method",
              "clazz" -> vt.typeObjCache(cls.tpe),
              "name" -> vt.internedStrings.getOrElseUpdate(
                m.sig.name, vt.toVirtObj(m.sig.name).address
              ),
              // Not sure why `&`ing this (and others) is necessary, but for some reason
              // ASM 5.2 parses the modifiers for java.util.Properties#save to be 131073,
              // (100000000000000001) but "normal" java reflect returns 1. The high-order
              // bit on the ASM modifiers value seems to be outside the allowed values
              // of method modifiers. No idea why it's there, but mask it out so the final
              // result matches what java.lang.reflect returns.
              "modifiers" -> new Ref.Raw(
                m.accessFlags &
                (
                  0x80 /*java.lang.reflect.Modifier.VARARGS*/ |
                  0x40 /*java.lang.reflect.Modifier.BRIDGE*/ |
                  0x1000 /*java.lang.reflect.Modifier.TRANSIENT*/ |
                  java.lang.reflect.Modifier.methodModifiers()
                )
              ),
              "returnType" -> vt.typeObjCache(m.sig.desc.ret),
              "parameterTypes" -> r.newArr("java.lang.Class",
                m.sig.desc.args.map(vt.typeObjCache)
              )
            ).address
          }
        )
      )()
    },
    native("java.lang.Class", "getEnclosingMethod0()[Ljava/lang/Object;"){(vt, arg) => 0},
    native("java.lang.Class", "getDeclaringClass()Ljava/lang/Class;"){(vt, arg) => 0},
    native("java.lang.Class", "getInterfaces()[Ljava/lang/Class;"){(vt, arg) =>

      val cls = vt.clsTable(
        vt.readAnyRef[String](vt.obj(arg()).apply("name"))
      )
      vt.alloc(implicit r =>
        r.newArr("java.lang.Class",
          cls.typeAncestry
            .filter(x => !cls.clsAncestry.contains(x))
            .toSeq
            .map(x => vt.typeObjCache(vt.clsTable(x).tpe))
        )
      ).address()
    },
    native("java.lang.Class", "getModifiers()I"){(vt, arg) =>

      val topClsName = vt.readAnyRef[String](vt.obj(arg()).apply("name"))

      vt.clsTable(topClsName).accessFlags
    },
    native("java.lang.Class", "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;").static {(vt, arg) =>

      vt.typeObjCache(imm.Type.readJava(vt.readAnyRef[String](arg())))()
    },
    native("java.lang.Class", "getSuperclass()Ljava/lang/Class;"){(vt, arg) =>

      val topClsName = vt.readAnyRef[String](vt.obj(arg()).apply("name"))

      vt.clsTable(topClsName)
        .superType
        .map(tpe => vt.typeObjCache(tpe)())
        .getOrElse(0)


    },

    native("java.lang.Class", "isArray()Z"){(vt, arg) =>

      if(vt.readAnyRef[String](vt.obj(arg()).apply("name")).contains('[')) 1 else 0

    },
    native("java.lang.Class", "isAssignableFrom(Ljava/lang/Class;)Z"){ (vt, arg) =>

      val clsA = vt.obj(arg())
      val clsB = vt.obj(arg())
      val nameA = vt.readAnyRef[String](clsA("name"))
      val nameB = vt.readAnyRef[String](clsB("name"))

      def check(s: imm.Type, t: imm.Type): Boolean = {

        (s, t) match{

          case (s: imm.Type.Cls, t: imm.Type.Cls) => vt.clsTable(t).typeAncestry.contains(s)
          case (s: imm.Type.Arr, imm.Type.Cls("java.lang.Object")) => true
          case (s: imm.Type.Arr, imm.Type.Cls("java.lang.Cloneable")) => true
          case (s: imm.Type.Arr, imm.Type.Cls("java.io.Serializable")) => true
          case (imm.Type.Arr(imm.Type.Prim(a)), imm.Type.Arr(imm.Type.Prim(b))) => a == b
          case (imm.Type.Arr(sc: imm.Type), imm.Type.Arr(tc: imm.Type)) => check(sc, tc)
          case _ => false
        }
      }
      if (check(imm.Type.read(nameA.replace('.', '/')), imm.Type.read(nameB.replace('.', '/')))) 1 else 0
    },
    native("java.lang.Class", "isInterface()Z"){(vt, arg) =>

      val clsObj = vt.obj(arg())
      val name = vt.readAnyRef[String](clsObj("name"))
      !Prim.allJava.contains(name) && vt.clsTable(name).isInterface
    },
    native("java.lang.Class", "isPrimitive()Z"){(vt, arg) =>

      val clsObj = vt.obj(arg())
      val name = vt.readAnyRef[String](clsObj("name"))
      val res = Prim.allJava.contains(name)
      if (res) 1 else 0
    },
    native("java.lang.Class", "registerNatives()V").static {(vt, arg) => },
    native("java.lang.ClassLoader", "resolveClass0(Ljava/lang/Class;)V").value(V)(()),
    native("java.lang.ClassLoader", "defineClass1(Ljava/lang/String;[BIILjava/security/ProtectionDomain;Ljava/lang/String;)Ljava/lang/Class;").func(I, I, I, I, I, I, I, I){
      (vt, classloader, name0, arr, offset, length, protectionDomain, src) =>
        val start = arr + Constants.arrayHeaderSize + offset
        val bytes = vt.heap.memory.slice(start, start + length).map(_.toByte)
        val name = vt.readAnyRef[String](name0)
        val cls = vt.clsTable.calcFromBytes(imm.Type.Cls(name), bytes)
        vt.typeObjCache(cls.tpe)()
    },
    native(
      "sun.misc.Unsafe",
      "defineClass(Ljava/lang/String;[BIILjava/lang/ClassLoader;Ljava/security/ProtectionDomain;)Ljava/lang/Class;").func(I, I, I, I, I, I, I, I){
      (vt, unsafe, name0, arr, offset, length, classloader, protectionDomain) =>
        val start = arr + Constants.arrayHeaderSize + offset
        val bytes = vt.heap.memory.slice(start, start + length).map(_.toByte)
        val name = vt.readAnyRef[String](name0)
        val cls = vt.clsTable.calcFromBytes(imm.Type.Cls(name), bytes)
        vt.typeObjCache(cls.tpe)()
    },
    native("java.lang.ClassLoader", "getCaller(I)Ljava/lang/Class;"){ (vt, arg) =>
      val name = arg() match{
        case 0 => "java.lang.ClassLoader"
        case 1 => vt.runningClassName(0)
        case 2 => vt.runningClassName(1)
      }
      vt.typeObjCache(imm.Type.readJava(name))()
    },
    native("java.lang.ClassLoader", "findLoadedClass0(Ljava/lang/String;)Ljava/lang/Class;").func(I, I, I) { (vt, clsLoader, str) =>
      val name = vt.readAnyRef[String](str)
      try{
        vt.clsTable.apply(name)
        vt.typeObjCache(name).apply()
      }catch{case e: rt.ClsTable.ClsNotFound =>
        0
      }
    },
    native("java.lang.ClassLoader", "findBootstrapClass(Ljava/lang/String;)Ljava/lang/Class;").func(I, I, I) { (vt, clsLoader, str) =>
      val name = vt.readAnyRef[String](str)
      try{
        vt.clsTable.apply(name)
        vt.typeObjCache(name).apply()
      }catch{case e: rt.ClsTable.ClsNotFound =>
        0
      }
    },
    native("java.lang.ClassLoader", "getSystemClassLoader()Ljava/lang/ClassLoader;").static.func(I) { (vt) =>
      vt.alloc{implicit r =>
        r.newObj("java.lang.ClassLoader")
      }
    },
    native("java.lang.ClassLoader", "getSystemResourceAsStream(Ljava/lang/String;)Ljava/io/InputStream;").static { (vt, arg) =>
      val name = vt.readAnyRef[String](arg())
      val stream = getClass.getResourceAsStream("/" + name)

      if (stream == null) 0
      else{
        val realResult = new DataInputStream(stream)

        val bytes = new Array[Byte](realResult.available())
        realResult.readFully(bytes)
        val byteStream = new ByteArrayInputStream(bytes)
        vt.alloc(vt.toVirtObj(byteStream)(_)).address()
      }
    },
    native("java.lang.invoke.MethodHandles$Lookup", "checkAccess(BLjava/lang/Class;Ljava/lang/invoke/MemberName;)V") {(vt, arg) =>
      // do nothing
    },
    native("java.lang.invoke.MethodHandleNatives", "registerNatives()V").static {(vt, arg) =>},
    native("java.lang.invoke.MethodHandleNatives", "verifyConstants()Z").static {(vt, arg) => 1},
    native("java.lang.invoke.MemberName", "vminfoIsConsistent()Z") {(vt, arg) => 1},
    native("java.lang.invoke.MethodHandleNatives", "staticFieldOffset(Ljava/lang/invoke/MemberName;)J").static.func(I, J) {
      (vt, memberName) =>
        val cls = vt.obj(memberName).apply("clazz")
        val name = vt.readAnyRef[String](vt.obj(memberName).apply("name"))
        val typeObj = vt.getTypeForTypeObj(cls)
        vt.clsTable(typeObj.asInstanceOf[imm.Type.Cls]).staticInfo.getIndex(name)
    },
    native("java.lang.invoke.MethodHandleNatives", "staticFieldBase(Ljava/lang/invoke/MemberName;)Ljava/lang/Object;").static.func(I, I) {
      (vt, memberName) =>
        val cls = vt.obj(memberName).apply("clazz")
        val typeObj = vt.getTypeForTypeObj(cls)
        val rtCls = vt.clsTable(typeObj.asInstanceOf[imm.Type.Cls])
        vt.checkInitialized(rtCls)
        rtCls.statics.apply()
    },
    native(
      "java.lang.invoke.MethodHandleNatives",
      "getMembers(Ljava/lang/Class;Ljava/lang/String;Ljava/lang/String;ILjava/lang/Class;I[Ljava/lang/invoke/MemberName;)I"
    ).static.func(I, I, I, I, I, I, I, I) { ()
      (vt, defc0, matchName0, matchSig0, matchFlags, caller0, skip0, results0) =>
        vt.alloc { implicit r =>
          val searchSuperclasses = 0 != (matchFlags & MHConstants.MN_SEARCH_SUPERCLASSES)
          val searchInterfaces = 0 != (matchFlags & MHConstants.MN_SEARCH_INTERFACES)

          val name = Option(vt.readAnyRef[String](matchName0))
          val sig = Option(vt.readAnyRef[String](matchSig0)).map(imm.Sig.read)
          val typeObj = vt.getTypeForTypeObj(defc0)
          val cls = vt.clsTable(typeObj.asInstanceOf[imm.Type.Cls])
          val results = vt.arr(results0)

          var count = 0
          var skip = skip0
          def addMatch(modifiers: Long,
                       typeSwitch: String,
                       nameStr: String,
                       typeString: String) = {

            if (count > results.length) () // do nothing
            else if (skip > 0) skip -= 1
            else {

              val (flags0, refKinds) = typeSwitch match {
                case "java.lang.reflect.Method" =>
                  val flags = MHConstants.MN_IS_METHOD
                  val refKinds =
                    if ((modifiers | MHConstants.ACC_INTERFACE) > 0) MHConstants.REF_invokeInterface
                    else if ((modifiers | MHConstants.ACC_STATIC) > 0) MHConstants.REF_invokeStatic
                    else MHConstants.REF_invokeVirtual
                  (flags, refKinds)

                case "java.lang.reflect.Constructor" =>
                  val flags = MHConstants.MN_IS_CONSTRUCTOR
                  val refKinds = MHConstants.REF_invokeSpecial
                  (flags, refKinds)

                case "java.lang.reflect.Field" =>
                  val flags = MHConstants.MN_IS_FIELD
                  val refKinds =
                    if ((modifiers | MHConstants.ACC_STATIC) > 0) MHConstants.REF_getStatic
                    else MHConstants.REF_getField
                  (flags, refKinds)
              }
              val flags = flags0 | (refKinds << MHConstants.MN_REFERENCE_KIND_SHIFT)
              val memberName = r.newObj("java.lang.invoke.MemberName",
                "clazz" -> Ref.Raw(defc0),
                "name" -> vt.toVirtObj(nameStr),
                "flags" -> Ref.Raw(flags),
                "type" -> vt.toVirtObj(typeString)
              )
              results(count) = memberName.address()
              count += 1

            }
          }

          if ((matchFlags & MHConstants.MN_IS_CONSTRUCTOR) != 0) {
            for (method <- cls.methods if method.sig.name == "<init>" && !sig.exists(_ != method.sig)) {
              addMatch(method.accessFlags,  "java.lang.reflect.Constructor", method.sig.name, method.sig.desc.toString)
            }
          } else if ((matchFlags & MHConstants.MN_IS_METHOD) != 0) {
            for {
              method <- cls.methods
              if method.clsIndex == cls.index
              if method.sig.name != "<init>" &&
                !name.exists(_ != method.sig.name) &&
                !sig.exists(_ != method.sig)
            } {
              addMatch(method.accessFlags, "java.lang.reflect.Method", method.sig.name, method.sig.desc.toString)
            }
          } else if ((matchFlags & MHConstants.MN_IS_FIELD) != 0) {
            for (field <- cls.fieldList0 if !name.exists(_ != field.name)) {
              addMatch(field.access, "java.lang.reflect.Field", field.name, field.desc.toString)
            }
          }
          count
        }

    },
    native("java.lang.invoke.MethodHandleNatives", "objectFieldOffset(Ljava/lang/invoke/MemberName;)J").static.func(I, J) {
      (vt, memberName) =>
        val address = vt.obj(memberName).apply("clazz")
        val name = vt.readAnyRef[String](vt.obj(memberName).apply("name"))
        val typeObj = vt.getTypeForTypeObj(address)
        vt.clsTable(typeObj.asInstanceOf[imm.Type.Cls]).fieldInfo.getIndex(name)
    },
    native(
      "java.lang.invoke.MethodHandleNatives",
      "init(Ljava/lang/invoke/MemberName;Ljava/lang/Object;)V"
    ).static.func(I, I, V) {(vt, memberName, ref) =>
      val modifiers = vt.obj(ref).apply("modifiers")

      val (flags0, refKinds) = vt.obj(ref).cls.tpe match {
        case imm.Type.Cls("java.lang.reflect.Method") =>
          if ((modifiers & MHConstants.ACC_INTERFACE) > 0) {
            (MHConstants.MN_IS_METHOD, MHConstants.REF_invokeInterface)
          } else if ((modifiers & MHConstants.ACC_STATIC) > 0) {
            (MHConstants.MN_IS_METHOD | MHConstants.ACC_STATIC, MHConstants.REF_invokeStatic)
          } else {
            (MHConstants.MN_IS_METHOD, MHConstants.REF_invokeVirtual)
          }

        case imm.Type.Cls("java.lang.reflect.Constructor") =>
          val flags = MHConstants.MN_IS_CONSTRUCTOR
          val refKinds = MHConstants.REF_invokeSpecial
          (flags, refKinds)

        case imm.Type.Cls("java.lang.reflect.Field") =>
          val flags = MHConstants.MN_IS_FIELD
          val refKinds =
            if ((modifiers & MHConstants.ACC_STATIC) > 0) MHConstants.REF_getStatic
            else MHConstants.REF_getField
          (flags, refKinds)
      }
      val flags = flags0 | (refKinds << MHConstants.MN_REFERENCE_KIND_SHIFT)

      vt.obj(memberName).update("clazz", vt.obj(ref).apply("clazz"))
      vt.obj(memberName).update("flags", flags)


      val params =
        vt.arr(vt.obj(ref).apply("parameterTypes")).map(vt.getTypeForTypeObj).map(_.internalName).mkString
      val ret = vt.getTypeForTypeObj(vt.obj(ref).apply("returnType")).internalName

      val str = "(" + params + ")" + ret

      vt.obj(memberName).update("type", vt.alloc{implicit r =>
        val addr = vt.toVirtObj(str)
        vt.internedStrings.getOrElseUpdate(str, addr).apply()
      })
      ()
    },
    native("java.lang.invoke.MethodHandleNatives", "getConstant(I)I").static {(vt, arg) => 9},
    native("java.lang.invoke.MethodHandleNatives", "resolve(Ljava/lang/invoke/MemberName;Ljava/lang/Class;)Ljava/lang/invoke/MemberName;").static.func(I, I, I) {
      (vt, memberName, cls0) =>


        val cls = vt.getTypeForTypeObj(vt.obj(memberName).apply("clazz"))

        val memberNameStr = vt.readAnyRef[String](vt.obj(memberName).apply("name"))



        val rtCls = vt.clsTable.apply(cls.asInstanceOf[imm.Type.Cls])
        val refFlag = (vt.obj(memberName)("flags") >> MHConstants.MN_REFERENCE_KIND_SHIFT) & MHConstants.MN_REFERENCE_KIND_MASK
        refFlag match {
          case MHConstants.REF_invokeInterface |
               MHConstants.REF_invokeVirtual |
               MHConstants.REF_invokeSpecial |
               MHConstants.REF_invokeStatic =>

            val tpe = vt.obj(memberName).apply("type")
            val ret = vt.getTypeForTypeObj(vt.obj(tpe).apply("rtype"))
            val params = vt.arr(vt.obj(tpe).apply("ptypes")).map(vt.getTypeForTypeObj)

            val sig = imm.Sig(memberNameStr, imm.Desc(Agg.from(params), ret))

            val actualMethod = refFlag match{
              case MHConstants.REF_invokeStatic =>
                if (rtCls.tpe.javaName == "java.lang.invoke.MethodHandle" &&
                  (memberNameStr == "linkToVirtual" ||
                    memberNameStr == "linkToStatic" ||
                    memberNameStr == "linkToSpecial" ||
                    memberNameStr == "linkToInterface")){
                  rtCls.staticTable.find(_.sig.name == memberNameStr)
                }else{
                  rtCls.staticTable.find(_.sig == sig)
                }
              case MHConstants.REF_invokeSpecial | MHConstants.REF_invokeInterface | MHConstants.REF_invokeVirtual =>
                if (rtCls.tpe.javaName == "java.lang.invoke.MethodHandle" &&
                  (memberNameStr == "invoke" ||
                    memberNameStr == "invokeExact" ||
                    memberNameStr == "invokeBasic")){
                  rtCls.vTable.find(_.sig.name == memberNameStr)
                }else {
                  rtCls.vTable.find(_.sig == sig)
                }

            }

            actualMethod match{
              case None => vt.throwExWithTrace("java.lang.NoSuchMethodError", "lol")
              case Some(actualMethod) =>
                vt.methodHandleMap(new Ref.UnsafeManual(memberName)) = actualMethod

                vt.obj(memberName)("flags") |= actualMethod.accessFlags
            }


          case MHConstants.REF_getField | MHConstants.REF_putField =>
            if (rtCls.fieldInfo.getIndex0(memberNameStr) == -1){
              vt.throwExWithTrace("java.lang.NoSuchFieldError", "lol")
            }else{
              vt.obj(memberName)("flags") |= rtCls.fieldInfo.get(memberNameStr).access
            }
          case MHConstants.REF_getStatic | MHConstants.REF_putStatic =>
            if (rtCls.staticInfo.getIndex0(memberNameStr) == -1) {
              vt.throwExWithTrace("java.lang.NoSuchFieldError", "lol")
            }else{
              vt.obj(memberName)("flags") |= rtCls.staticInfo.get(memberNameStr).access
            }
        }

        memberName
    },
    native("java.lang.ClassLoader", "registerNatives()V").static {(vt, arg) =>},
    native("java.lang.ClassLoader", "initSystemClassLoader()V").static.value(V)(()),
    native("java.lang.Double", "doubleToRawLongBits(D)J").static{(vt, arg) => imm.Type.Prim.J.read(arg)},
    native("java.lang.Double", "longBitsToDouble(J)D").static{(vt, arg) => imm.Type.Prim.J.read(arg)},
    native("java.lang.Float", "intBitsToFloat(I)F").static{(vt, arg) => arg()},
    native("java.lang.Float", "floatToRawIntBits(F)I").static{(vt, arg) => arg()},
    native("java.lang.Object", "clone()Ljava/lang/Object;"){(vt, arg) =>
      val original = arg()
      val heapSize =
        if (vt.isObj(original)) vt.obj(original).cls.heapSize
        else if (vt.isArr(original)) vt.arr(original).heapSize
        else ???

      val cloned = vt.heap.allocate(heapSize)
      for(i <- 0 until heapSize){
        vt.heap(cloned + i) = vt.heap(original + i)
      }

      cloned
    },
    native("java.lang.Object", "getClass()Ljava/lang/Class;"){ (vt, arg) =>
      val value = arg()
      val string =
        if(vt.isObj(value)) vt.obj(value).cls.tpe.javaName
        else vt.arr(value).tpe.javaName

      vt.typeObjCache(imm.Type.readJava(string))()
    },

    native("java.lang.Object", "hashCode()I"){(vt, arg) => arg()},
    native("java.lang.Object", "registerNatives()V").static{(vt, arg) => },
    native("java.lang.Runtime", "freeMemory()J"){(vt, arg) => 4*1024*1024},
    native("java.lang.Runtime", "availableProcessors()I"){(vt, arg) => 1},
    native("java.lang.System", "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V").static{ (vt, arg) =>
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

    native("java.lang.System", "identityHashCode(Ljava/lang/Object;)I").static{(vt, arg) => arg()},
    native("java.lang.System", "nanoTime()J").static{(vt, arg) => System.nanoTime()},
    native("java.lang.System", "currentTimeMillis()J").static{(vt, arg) =>System.currentTimeMillis()},
    native("java.lang.System", "getProperty(Ljava/lang/String;)Ljava/lang/String;").static{(vt, arg) => 0},
    native("java.lang.System", "getProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;").static{(vt, arg) => 0},
    native("java.lang.System", "registerNatives()V").static{(vt, arg) =>},
    native("java.lang.String", "intern()Ljava/lang/String;"){ (vt, arg) =>
      val addr = arg()
      val str = vt.readAnyRef[String](addr)
      val result = vt.internedStrings.getOrElseUpdate(str, new Ref.UnsafeManual(addr))
      result()
    },
    native("java.lang.Thread", "registerNatives()V").static{(vt, arg) => ()},
    native("java.lang.Thread", "currentThread()Ljava/lang/Thread;").static{(vt, arg) => vt.currentThread},
    native("java.lang.Thread", "setPriority0(I)V"){(vt, arg) => ()},
    native("java.lang.Thread", "isAlive()Z"){(vt, arg) => false},
    native("java.lang.Thread", "start0()V"){(vt, arg) => ()},
    native("java.lang.Throwable", "fillInStackTrace()Ljava/lang/Throwable;"){ (vt, arg) =>
      val throwable = vt.obj(arg())
      val trace = vt.trace
      throwable("stackTrace") = vt.alloc(vt.toVirtObj(trace)(_))
//      val inner = vt.toRealObj[Throwable](throwable.address())
//      assert(
//        inner.getStackTrace.length == trace.length,
//        s"${inner.getStackTrace.length} == ${trace.length}"
//      )
      throwable.address()
    },
    native("java.lang.reflect.Array", "newArray(Ljava/lang/Class;I)Ljava/lang/Object;").static{ (vt, arg) =>
      val (cls, length) = (arg(), arg())

      val clsObj = vt.obj(cls)
      val clsName = vt.readAnyRef[String](clsObj("name"))
      vt.alloc(_.newArr(imm.Type.readJava(clsName), length)).address()
    },
    native("java.lang.reflect.Array", "set(Ljava/lang/Object;ILjava/lang/Object;)V").static{ (vt, arg) =>
      val (arr, index, obj) = (arg(), arg(), arg())
      vt.invoke0(
        "metascala/patches/java/lang/reflect/Array",
        imm.Sig.read("set(Ljava/lang/Object;ILjava/lang/Object;)V"),
        Agg(arr, index, obj)
      )
    },
    native("java.lang.reflect.Constructor", "newInstance([Ljava/lang/Object;)Ljava/lang/Object;"){
      (vt, arg) => vt.newInstance(arg(), arg())
    },
    native("java.lang.reflect.NativeConstructorAccessorImpl", "newInstance0(Ljava/lang/reflect/Constructor;[Ljava/lang/Object;)Ljava/lang/Object;").static{
      (vt, arg) =>
        val (cons, args) = (arg(), arg())

        val name = vt.readAnyRef[String](vt.obj(vt.obj(cons).apply("clazz")).apply("name"))
        vt.alloc(_.newObj(name)).address()
    },
    native("java.lang.StrictMath", "sin(D)D").static.func(D, D){ (vt, arg) => StrictMath.sin(arg)},
    native("java.lang.StrictMath", "cos(D)D").static.func(D, D){ (vt, arg) => StrictMath.cos(arg)},
    native("java.lang.StrictMath", "tan(D)D").static.func(D, D){ (vt, arg) => StrictMath.tan(arg)},
    native("java.lang.StrictMath", "asin(D)D").static.func(D, D){ (vt, arg) => StrictMath.asin(arg)},
    native("java.lang.StrictMath", "acos(D)D").static.func(D, D){ (vt, arg) => StrictMath.acos(arg)},
    native("java.lang.StrictMath", "atan(D)D").static.func(D, D){ (vt, arg) => StrictMath.atan(arg)},
    native("java.lang.StrictMath", "toRadians(D)D").static.func(D, D){ (vt, arg) => StrictMath.toRadians(arg)},
    native("java.lang.StrictMath", "toDegrees(D)D").static.func(D, D){ (vt, arg) => StrictMath.toDegrees(arg)},
    native("java.lang.StrictMath", "exp(D)D").static.func(D, D){ (vt, arg) => StrictMath.exp(arg)},
    native("java.lang.StrictMath", "log(D)D").static.func(D, D){ (vt, arg) => StrictMath.log(arg)},
    native("java.lang.StrictMath", "log10(D)D").static.func(D, D){ (vt, arg) => StrictMath.log10(arg)},
    native("java.lang.StrictMath", "sqrt(D)D").static.func(D, D){ (vt, arg) => StrictMath.sqrt(arg)},
    native("java.lang.StrictMath", "cbrt(D)D").static.func(D, D){ (vt, arg) => StrictMath.cbrt(arg)},
    native("java.lang.StrictMath", "IEEEremainder(DD)D").static.func(D, D, D){ (vt, arg1, arg2) => StrictMath.IEEEremainder(arg1, arg2)},
    native("java.lang.StrictMath", "atan2(DD)D").static.func(D, D, D){ (vt, arg1, arg2) => StrictMath.atan2(arg1, arg2)},
    native("java.lang.StrictMath", "pow(DD)D").static.func(D, D, D) { (vt, arg1, arg2) => StrictMath.pow(arg1, arg2)},
    native("java.lang.StrictMath", "sinh(D)D").static.func(D, D){ (vt, arg) => StrictMath.sinh(arg)},
    native("java.lang.StrictMath", "cosh(D)D").static.func(D, D){ (vt, arg) => StrictMath.cosh(arg)},
    native("java.lang.StrictMath", "tanh(D)D").static.func(D, D){ (vt, arg) => StrictMath.tanh(arg)},
    native("java.lang.StrictMath", "hypot(DD)D").static.func(D, D, D) { (vt, arg1, arg2) => StrictMath.hypot(arg1, arg2)},
    native("java.lang.StrictMath", "expm1(D)D").static.func(D, D){ (vt, arg) => StrictMath.expm1(arg)},
    native("java.lang.StrictMath", "log1p(D)D").static.func(D, D){ (vt, arg) => StrictMath.log1p(arg)},

    native("java.nio.charset.Charset", "defaultCharset()Ljava/nio/charset/Charset;").static{(vt, arg) =>
      vt.invoke0(
        "metascala.DummyCharset",
        imm.Sig.read("getValue()Ljava/nio/charset/Charset;"),
        Agg.empty
      )

      vt.returnedVal(0)
    },
    native("java.security.AccessController", "doPrivileged(Ljava/security/PrivilegedExceptionAction;)Ljava/lang/Object;").static{ (vt, arg) =>
      vt.invokeRun(arg())
    },
    native("java.security.AccessController", "doPrivileged(Ljava/security/PrivilegedAction;)Ljava/lang/Object;").static{ (vt, arg) =>
      vt.invokeRun(arg())
    },
    native("java.security.AccessController", "doPrivileged(Ljava/security/PrivilegedAction;Ljava/security/AccessControlContext;)Ljava/lang/Object;").static{ (vt, arg) =>
      vt.invokeRun(arg())
    },
    native("java.security.AccessController", "getStackAccessControlContext()Ljava/security/AccessControlContext;").static{(vt, arg) => 0},
    native("java.util.concurrent.atomic.AtomicLong", "VMSupportsCS8()Z").static.value(Z)(true),
    native("java/util/ServiceLoader$1", "hasNext()Z;").value(Z)(false),
    native("java.util.TimeZone", "getDefault()Ljava/util/TimeZone;").static.func(I){ vt =>
      vt.alloc(r =>
        r.newObj("java.util.SimpleTimeZone",
          "rawOffset" -> Ref.Raw(0),
          "ID" -> vt.toVirtObj("GMT+08:00")(r).address
        )
      ).address()
    },
    native("java.util.TimeZone", "getSystemTimeZoneID(Ljava/lang/String;)Ljava/lang/String;").static.value(I)(0),
    native("java.util.TimeZone", "getSystemGMTOffsetID()Ljava/lang/String;").static.func(I){ vt =>
      vt.alloc(r => vt.toVirtObj("GMT+08:00")(r)).address()
    },
    native("scala.Predef$", "println(Ljava/lang/Object;)V").func(I, I, V){ (vt, predef, o) =>

      if (o == 0) println("Virtual\t" + null)
      else {
        vt.invoke1("java.lang.Object", imm.Sig.read("toString()Ljava/lang/String;"), Agg(o))
        println("Virtual\t" + vt.readAnyRef[String](vt.returnedVal(0)))
      }
    },
    native("sun.misc.Hashing", "randomHashSeed(Ljava/lang/Object;)I").value(I)(31337), // sufficiently random
    native("sun.misc.Unsafe", "allocateMemory(J)J").func(I, J, J){ (vt, unsafe, size) =>
      val res = vt.offHeapPointer
      vt.setOffHeapPointer(vt.offHeapPointer + size)
      res
    },
    native(
      "sun.misc.Unsafe",
      "defineAnonymousClass(Ljava/lang/Class;[B[Ljava/lang/Object;)Ljava/lang/Class;"
    ).func(I, I, I, I, I){ (vt, unsafe, hostCls0, bytes0, cpPatches0) =>

      val bytesArray = vt.arr(bytes0)
      val start = bytes0 + Constants.arrayHeaderSize
      val bytes = vt.heap.memory.slice(start, start + bytesArray.length).map(_.toByte)
      val name = "AnonymousClass" + math.abs(scala.util.Random.nextInt())
      val cpMap = vt.arr(cpPatches0)
        .zipWithIndex
        .collect{case (addr, index) if addr != 0 => (index, addr)}
        .toMap

      val cls = vt.clsTable.calcFromBytes(imm.Type.Cls(name), bytes, cpMap)

      // We need to apply the constant pool patches to the parsed classfiles
      //
      // https://blogs.oracle.com/jrose/anonymous-classes-in-the-vm
      if (cls.methods.exists(_.sig.name == "getBooleanStaticInit")) {
        pprint.log(cpPatches0)
        pprint.log(vt.arr(cpPatches0).length)
        pprint.log(vt.arr(cpPatches0))
        for ((p, i) <- vt.arr(cpPatches0).zipWithIndex) {
          if (p != 0) println("patching " + i + " to " + vt.obj(p).cls)
        }
      }

      vt.typeObjCache(name)()
    },
    native("sun.misc.Unsafe", "freeMemory(J)V").value(V)(()),// Do nothing lol
    native("sun.misc.Unsafe", "putLong(JJ)V").func(I, J, J, V){ (vt, unsafe, offset, value) =>
      val bs = ByteBuffer.allocate(8)
      bs.putLong(value)

      for(i <- 0 until 8) {
        vt.offHeap(offset.toInt + i) = bs.get(i)
      }
      ()
    },
    native("sun.misc.Unsafe", "getByte(J)B").func(I, J, B){ (vt, unsafe, offset) =>
      val res = vt.offHeap(offset.toInt)
      res
    },
    native("sun.misc.Unsafe", "arrayBaseOffset(Ljava/lang/Class;)I").value(I)(0),
    native("sun.misc.Unsafe", "arrayIndexScale(Ljava/lang/Class;)I").value(I)(1),
    native("sun.misc.Unsafe", "allocateInstance(Ljava/lang/Class;)Ljava/lang/Object;").func(I, I, I){ (vt, unsafe, clsPtr) =>

      val name = vt.readAnyRef[String](vt.obj(clsPtr).apply("name"))
      vt.alloc(_.newObj(name)).address()
    },
    native("sun.misc.Unsafe", "addressSize()I").value(I)(4),
    native("sun.misc.Unsafe", "compareAndSwapInt(Ljava/lang/Object;JII)Z").func(I, I, J, I, I, Z){
      compareAndSwapWord(_, _, _, _, _, _, I)
    },
    native("sun.misc.Unsafe", "compareAndSwapObject(Ljava/lang/Object;JLjava/lang/Object;Ljava/lang/Object;)Z").func(I, I, J, I, I, Z){
      compareAndSwapWord(_, _, _, _, _, _, I)
    },
    native("sun.misc.Unsafe", "compareAndSwapLong(Ljava/lang/Object;JJJ)Z").func(I, I, J, J, J, Z){
      compareAndSwapWord(_, _, _, _, _, _, J)
    },
    native("sun.misc.Unsafe", "ensureClassInitialized(Ljava/lang/Class;)V").value(V)(()),

    native("sun.misc.Unsafe", "getBooleanVolatile(Ljava/lang/Object;J)Z").func(I, I, J, Z){
      unsafeGet(_, _, _, _, Z)
    },
    native("sun.misc.Unsafe", "getBoolean(Ljava/lang/Object;J)Z").func(I, I, J, Z){
      unsafeGet(_, _, _, _, Z)
    },
    native("sun.misc.Unsafe", "putBooleanVolatile(Ljava/lang/Object;JZ)V").func(I, I, J, Z, V){
      unsafePut(_, _, _, _, _, Z)
    },
    native("sun.misc.Unsafe", "putBoolean(Ljava/lang/Object;JZ)V").func(I, I, J, Z, V){
      unsafePut(_, _, _, _, _, Z)
    },
    native("sun.misc.Unsafe", "getByteVolatile(Ljava/lang/Object;J)B").func(I, I, J, B){
      unsafeGet(_, _, _, _, B)
    },
    native("sun.misc.Unsafe", "getByte(Ljava/lang/Object;J)B").func(I, I, J, B){
      unsafeGet(_, _, _, _, B)
    },
    native("sun.misc.Unsafe", "putByteVolatile(Ljava/lang/Object;JB)V").func(I, I, J, B, V){
      unsafePut(_, _, _, _, _, B)
    },
    native("sun.misc.Unsafe", "putByte(Ljava/lang/Object;JB)V").func(I, I, J, B, V){
      unsafePut(_, _, _, _, _, B)
    },
    native("sun.misc.Unsafe", "getCharVolatile(Ljava/lang/Object;J)C").func(I, I, J, C){
      unsafeGet(_, _, _, _, C)
    },
    native("sun.misc.Unsafe", "getChar(Ljava/lang/Object;J)C").func(I, I, J, C){
      unsafeGet(_, _, _, _, C)
    },
    native("sun.misc.Unsafe", "putCharVolatile(Ljava/lang/Object;JC)V").func(I, I, J, C, V){
      unsafePut(_, _, _, _, _, C)
    },
    native("sun.misc.Unsafe", "putChar(Ljava/lang/Object;JC)V").func(I, I, J, C, V){
      unsafePut(_, _, _, _, _, C)
    },
    native("sun.misc.Unsafe", "getShortVolatile(Ljava/lang/Object;J)S").func(I, I, J, S){
      unsafeGet(_, _, _, _, S)
    },
    native("sun.misc.Unsafe", "getShort(Ljava/lang/Object;J)S").func(I, I, J, S){
      unsafeGet(_, _, _, _, S)
    },
    native("sun.misc.Unsafe", "putShortVolatile(Ljava/lang/Object;JS)V").func(I, I, J, S, V){
      unsafePut(_, _, _, _, _, S)
    },
    native("sun.misc.Unsafe", "putShort(Ljava/lang/Object;JS)V").func(I, I, J, S, V){
      unsafePut(_, _, _, _, _, S)
    },
    native("sun.misc.Unsafe", "getIntVolatile(Ljava/lang/Object;J)I").func(I, I, J, I){
      unsafeGet(_, _, _, _, I)
    },
    native("sun.misc.Unsafe", "getInt(Ljava/lang/Object;J)I").func(I, I, J, I){
      unsafeGet(_, _, _, _, I)
    },
    native("sun.misc.Unsafe", "putIntVolatile(Ljava/lang/Object;JI)V").func(I, I, J, I, V){
      unsafePut(_, _, _, _, _, I)
    },
    native("sun.misc.Unsafe", "putInt(Ljava/lang/Object;JI)V").func(I, I, J, I, V){
      unsafePut(_, _, _, _, _, I)
    },
    native("sun.misc.Unsafe", "getFloatVolatile(Ljava/lang/Object;J)F").func(I, I, J, F){
      unsafeGet(_, _, _, _, F)
    },
    native("sun.misc.Unsafe", "getFloat(Ljava/lang/Object;J)F").func(I, I, J, F){
      unsafeGet(_, _, _, _, F)
    },
    native("sun.misc.Unsafe", "putFloatVolatile(Ljava/lang/Object;JF)V").func(I, I, J, F, V){
      unsafePut(_, _, _, _, _, F)
    },
    native("sun.misc.Unsafe", "putFloat(Ljava/lang/Object;JF)V").func(I, I, J, F, V){
      unsafePut(_, _, _, _, _, F)
    },
    native("sun.misc.Unsafe", "getLongVolatile(Ljava/lang/Object;J)J").func(I, I, J, J){
      unsafeGet(_, _, _, _, J)
    },
    native("sun.misc.Unsafe", "getLong(Ljava/lang/Object;J)J").func(I, I, J, J){
      unsafeGet(_, _, _, _, J)
    },
    native("sun.misc.Unsafe", "putLongVolatile(Ljava/lang/Object;JJ)V").func(I, I, J, J, V){
      unsafePut(_, _, _, _, _, J)
    },
    native("sun.misc.Unsafe", "putLong(Ljava/lang/Object;JJ)V").func(I, I, J, J, V){
      unsafePut(_, _, _, _, _, J)
    },
    native("sun.misc.Unsafe", "getDoubleVolatile(Ljava/lang/Object;J)D").func(I, I, J, D){
      unsafeGet(_, _, _, _, D)
    },
    native("sun.misc.Unsafe", "getDouble(Ljava/lang/Object;J)D").func(I, I, J, D){
      unsafeGet(_, _, _, _, D)
    },
    native("sun.misc.Unsafe", "putDoubleVolatile(Ljava/lang/Object;JD)V").func(I, I, J, D, V){
      unsafePut(_, _, _, _, _, D)
    },
    native("sun.misc.Unsafe", "putDouble(Ljava/lang/Object;JD)V").func(I, I, J, D, V){
      unsafePut(_, _, _, _, _, D)
    },
    native("sun.misc.Unsafe", "getObjectVolatile(Ljava/lang/Object;J)Ljava/lang/Object;").func(I, I, J, I){
      unsafeGet(_, _, _, _, I)
    },
    native("sun.misc.Unsafe", "getObject(Ljava/lang/Object;J)Ljava/lang/Object;").func(I, I, J, I){
      unsafeGet(_, _, _, _, I)
    },
    native("sun.misc.Unsafe", "putObjectVolatile(Ljava/lang/Object;JLjava/lang/Object;)V").func(I, I, J, I, V){
      unsafePut(_, _, _, _, _, I)
    },
    native("sun.misc.Unsafe", "putObject(Ljava/lang/Object;JLjava/lang/Object;)V").func(I, I, J, I, V){
      unsafePut(_, _, _, _, _, I)
    },
    native("sun.misc.Unsafe", "putOrderedObject(Ljava/lang/Object;JLjava/lang/Object;)V").func(I, I, J, I, V){
      (vt, unsafe, o, offset, ref) =>
        if (vt.isObj(o)) vt.obj(o).members(offset.toInt) = ref
        else vt.arr(o)(offset.toInt) = ref
    },
    native("sun.misc.Unsafe", "objectFieldOffset(Ljava/lang/reflect/Field;)J").func(I, I, J){
      (vt, unsafe, f) => vt.obj(f).apply("slot")
    },
    native("sun.misc.Unsafe", "shouldBeInitialized(Ljava/lang/Class;)Z").func(I, I, Z){
      (vt, unsafe, cls) =>
        !vt.clsTable(vt.getTypeForTypeObj(cls).asInstanceOf[imm.Type.Cls]).initialized
    },
    native("sun.misc.Unsafe", "staticFieldOffset(Ljava/lang/reflect/Field;)J").func(I, I, J){
      (vt, unsafe, f) =>vt.obj(f).apply("slot")
    },
    native("sun.misc.Unsafe", "staticFieldBase(Ljava/lang/reflect/Field;)Ljava/lang/Object;").func(I, I, I){
      (vt, unsafe, f) =>
        val clsObj = vt.obj(vt.obj(f).apply("clazz"))
        val clsName = vt.readAnyRef[String](clsObj.apply("name"))
        val rtCls = vt.clsTable(clsName)
        vt.checkInitialized(rtCls)
        rtCls.statics()
    },
    native("sun.misc.Unsafe", "registerNatives()V").value(V)(()),
    native("sun.misc.Unsafe", "getUnsafe()Lsun/misc/Unsafe;").static.func(I){vt => vt.theUnsafe.address()},
    native("sun.misc.Unsafe", "<clinit>()V").static.value(V)(()),
    native("sun.misc.URLClassPath", "getLookupCacheURLs(Ljava/lang/ClassLoader;)[Ljava/net/URL;").static.func(I, I){
      (vt, classLoader) => vt.alloc(r => r.newArr("java.net.URL", 0)).address()
    },
    native("sun.misc.VM", "getSavedProperty(Ljava/lang/String;)Ljava/lang/String;").static.value(I)(0),
    native("sun.misc.VM", "initialize()V").static.value(V)(()),
    native(
      "java.util.EnumMap",
      "getKeyUniverse(Ljava/lang/Class;)[Ljava/lang/Enum;"
    ).static.func(I, I) {
      (vt, cls0) =>

        val cls = vt.getTypeForTypeObj(cls0)
        vt.invoke1(cls.asInstanceOf[imm.Type.Cls], imm.Sig.read("values()[" + cls.internalName), Agg(cls0))
        vt.returnedVal(0)
    },
    native("sun.reflect.NativeMethodAccessorImpl", "invoke0(Ljava/lang/reflect/Method;Ljava/lang/Object;[Ljava/lang/Object;)Ljava/lang/Object;")
      .static.func(I, I, I, I){ (vt, m0, this0, argsArr0) =>

      val name = vt.readAnyRef[String](vt.obj(m0).apply("name"))
      val ret = vt.getTypeForTypeObj(vt.obj(m0).apply("returnType"))
      val params = vt.arr(vt.obj(m0).apply("parameterTypes")).map(vt.getTypeForTypeObj)
      val cls = vt.getTypeForTypeObj(vt.obj(m0).apply("clazz")).asInstanceOf[imm.Type.Cls]
      val rtCls = vt.clsTable(cls)

      val mRef = rtCls.method(name, imm.Desc(Agg.from(params), ret)).get
      val unboxedArgs = for((v, i) <- Agg.from(vt.arr(argsArr0)).zipWithIndex) yield {
        mRef.sig.desc.args(i) match{
          case p: imm.Type.Prim[_] =>
            vt.obj(v).members
          case _ => Seq(v)
        }
      }

      val thisCell = if (mRef.static) Agg() else Agg(this0)

      vt.invoke1(cls, imm.Sig(name, imm.Desc(Agg.from(params), ret)), thisCell ++ unboxedArgs.flatten)
      ret match{
        case p: imm.Type.Prim[_] =>
          vt.alloc{implicit r =>
            vt.boxIt(p, Util.reader(vt.returnedVal, 0)).apply()
          }
        case _ => vt.returnedVal(0)
      }

    },
    native("sun.reflect.Reflection", "filterFields(Ljava/lang/Class;[Ljava/lang/reflect/Field;)[Ljava/lang/reflect/Field;").static.func(I, I, I){ (vt, cls, fs) =>
      fs
    },
    native("sun.reflect.Reflection", "getCallerClass(I)Ljava/lang/Class;").static.func(I, I){ (vt, n) =>
      getCallerClass(vt, n)
    },
    native("sun.reflect.Reflection", "getCallerClass()Ljava/lang/Class;").static.func(I){ (vt) =>
      getCallerClass(vt, 1)
    },
    native("sun.reflect.Reflection", "getClassAccessFlags(Ljava/lang/Class;)I").static.func(I, I){ (vt, o) =>

      val addr = vt.obj(o).apply("name")
      val str = vt.readAnyRef[String](addr)
      val res = vt.clsTable(str).accessFlags
      res
    },
    native("metascala.Fail", "indirectMissingNativeMethodImplementation()V").func(V) { vt =>
      vt.invoke1("metascala.Fail", imm.Sig.read("indirectMissingNativeMethodImplementation0(I)I"), Agg(31337))
    },
    native("metascala.Virtualizer$", "unsafe()Lsun/misc/Unsafe;").func(I){vt =>
      vt.theUnsafe.address()
    }
  )


  def unsafeCheck[T](vt: Bindings.Interface,
                     o: Int,
                     offset: Long,
                     prim: imm.Type.Prim[T]) = {
    assert(o != 0)
    if (vt.isObj(o)){
      assert(vt.obj(o).cls.fieldInfo.get(offset.toInt).desc.size == prim.size)
    }else /*(vt.isArr(o))*/{
      assert(vt.arr(o).innerType.size == prim.size)
      assert(vt.arr(o).length > offset + prim.size - 1)
    }
  }

  def compareAndSwapWord[T](vt: Bindings.Interface,
                            unsafe: Int,
                            o: Int,
                            slot: Long,
                            expected: T,
                            x: T,
                            prim: imm.Type.Prim[T]) = {
    unsafeCheck(vt, o, slot, prim)
    val obj = vt.obj(o)
    val current = prim.read(Util.reader(obj.members, slot.toInt))
    if (current == expected){
      prim.write(x, Util.writer(obj.members, slot.toInt))
      true
    }else{
      false
    }
  }
  def getCallerClass(vt: Bindings.Interface, n: Int) = {
    if (n >= vt.threadStackLength) 0
    else {
      val name = vt.runningClassName(n)
      vt.typeObjCache(imm.Type.readJava(name))()
    }
  }
  def unsafeGet[T](vt: Bindings.Interface,
                   unsafe0: Int,
                   o: Int,
                   offset: Long,
                   prim: imm.Type.Prim[T]): T = {
    unsafeCheck(vt, o, offset, prim)
    prim.read(Util.reader(vt.obj(o).members, offset.toInt))
  }
  def unsafePut[T](vt: Bindings.Interface,
                   unsafe0: Int,
                   o: Int,
                   offset: Long,
                   value: T,
                   prim: imm.Type.Prim[T]): Unit = {
    unsafeCheck(vt, o, offset, prim)
    prim.write(value, Util.writer(vt.obj(o).members, offset.toInt))
  }
}
