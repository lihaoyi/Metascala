package svm.parsing

import org.scalatest.FreeSpec
import svm.parsing.Attribute.{Exceptions, LineNumberTable, Code, SourceFile}
import java.nio.ByteBuffer
import svm.parsing.ConstantInfo.{ZeroConstant, ClassRef, Utf8Info}
import java.io.DataInputStream
import svm.parsing.Attribute.LineNumberTable.LineNumberData
import svm.parsing.Util.Print
import svm.Access
import svm.parsing.opcodes.OpCodes

class ClassloaderTests extends FreeSpec{


  "loading class files" - {
    "hello world" in {

      val files = Util.compile("helloworld")
      val classData = ClassFile.read(ByteBuffer.wrap(files("helloworld.HelloWorld")))
      Util.printClass(files("helloworld.HelloWorld"))
      implicit val constantPool = classData.constant_pool
      val AccessFlags = (Access.Super | Access.Public)
      import OpCodes._
      val ClassFile(
        0xcafebabe,  // magic
        0,  // minor_version
        51, // major_version
        _,  // constant_pool
        AccessFlags,  // access_flags
        ClassRef("helloworld/HelloWorld"),  // this_class
        ClassRef("java/lang/Object"),  // super_class
        Nil,  // interfaces
        Nil,  // fields
        Seq(
          MethodInfo(_,
            "<init>",
            "()V",
            Seq(Code(_, _, initBytes, _, _))
          ),
          MethodInfo(_,
            "main",
            "([Ljava/lang/String;)V",
            Seq(Code(_, _,
              Seq(GetStatic.id, _, _, Ldc.id, _, InvokeVirtual.id, _, _, Return.id),
            _, _))
          )
        ),  // methods
        Seq(SourceFile("HelloWorld.java"))   // attributes
      ) = classData

    }
    "java.lang.Object" in {
      val classBytes = getClass.getResourceAsStream("/classpath/java/lang/Object.class")
      val byteArray = new Array[Byte](classBytes.available())
      new DataInputStream(classBytes).readFully(byteArray)
      //Util.printClass(byteArray)
      val classData = ClassFile.read(ByteBuffer.wrap(byteArray))
      implicit val constantPool = classData.constant_pool
      import Access._
      val PublicSuper = (Super | Public)
      val PublicNative = Public | Native
      val PublicFinal = Public | Final
      val PublicNativeFinal = PublicNative | Final
      val PrivateStaticNative = Private | Static | Native
      val ClassFile(
        0xcafebabe,  // magic
        0,  // minor_version
        51, // major_version
        _,  // constant_pool
        PublicSuper,  // access_flags
        ClassRef("java/lang/Object"),  // this_class
        null,  // super_class
        Nil,  // interfaces
        Nil,  // fields
        Seq(
          MethodInfo(Public, "<init>", "()V", Seq(
            Code(_, _, Seq(-79), Nil, Seq(LineNumberTable(Seq(LineNumberData(_, _)))))
          )),
          MethodInfo(Protected, "clone", "()Ljava/lang/Object;",
            Seq(Code(_, _, _, _, _), Exceptions(Seq(ClassRef("java/lang/CloneNotSupportedException"))))
          ),
          MethodInfo(PrivateStaticNative, "clone", "(Ljava/lang/Object;)Ljava/lang/Object;", Nil),
          MethodInfo(Access.Public, "equals", "(Ljava/lang/Object;)Z", _),
          MethodInfo(Access.Protected, "finalize", "()V", _),
          MethodInfo(PublicFinal, "getClass", _, _),
          MethodInfo(PrivateStaticNative, "getVMClass", "(Ljava/lang/Object;)Lavian/VMClass;", Nil),
          MethodInfo(PublicNative, "hashCode", "()I", Nil),
          MethodInfo(PublicNativeFinal, "notify", "()V", Nil),
          MethodInfo(PublicNativeFinal, "notifyAll", "()V", Nil),
          MethodInfo(PublicNative, "toString", "()Ljava/lang/String;", Nil),
          MethodInfo(PublicFinal, "wait", "()V", _),
          MethodInfo(PublicNativeFinal, "wait", "(J)V", _),
          MethodInfo(PublicFinal, "wait", "(JI)V", _)
        ),  // methods
        _   // attributes
      ) = classData

    }
  }
  "TypeDescriptor tests" - {
    "hello word" in {
      val TypeDesc(Seq(), "V") = TypeDesc.read("()V")
    }
    "more" in {
      val TypeDesc(Seq("J", "Z"), "I") = TypeDesc.read("(JZ)I")
      val TypeDesc(Seq("I", "F"), "V") = TypeDesc.read("(IF)V")
    }
    "more with objects" in {
      val TypeDesc(Seq("Ljava/lang/Object;"), "I") = TypeDesc.read("(Ljava/lang/Object;)I")
      val TypeDesc(Seq("I", "Ljava/lang/String;"), "[I") = TypeDesc.read("(ILjava/lang/String;)[I")
      val TypeDesc(Seq("[I"), "Ljava/lang/Object;") = TypeDesc.read("([I)Ljava/lang/Object;")
      val TypeDesc(
        Seq("I", "D", "Ljava/lang/Thread;"),
        "Ljava/lang/Object;"
      ) = TypeDesc.read("(IDLjava/lang/Thread;)Ljava/lang/Object;")
    }


  }
}
