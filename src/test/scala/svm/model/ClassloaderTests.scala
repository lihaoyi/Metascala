package svm.model

import org.scalatest.FreeSpec
import svm.model.AttributeInfo.{Exceptions, LineNumberTable, Code, SourceFile}
import java.nio.ByteBuffer
import svm.model.ConstantInfo.{ClassRef, Utf8}
import java.io.DataInputStream
import svm.model.AttributeInfo.LineNumberTable.LineNumberData
import svm.model.Util.Print

class ClassloaderTests extends FreeSpec{
  object Cp{ def unapply(index: Short)(implicit cp: Seq[ConstantInfo]): Option[ConstantInfo] = {
    cp.lift(index)
  }}


  "hello world" in {

    val files = Util.compile("helloworld")
    val classData = ClassFile.read(ByteBuffer.wrap(files("helloworld.HelloWorld")))
    Util.printClass(files("helloworld.HelloWorld"))
    implicit val constantPool = classData.constant_pool
    val AccessFlags = (Access.Super | Access.Public)
    val ClassFile(
      0xcafebabe,  // magic
      0,  // minor_version
      51, // major_version
      _,  // constant_pool
      AccessFlags,  // access_flags
      Cp(ClassRef(Cp(Utf8("helloworld/HelloWorld")))),  // this_class
      Cp(ClassRef(Cp(Utf8("java/lang/Object")))),  // super_class
      Nil,  // interfaces
      Nil,  // fields
      Seq(
        MethodInfo(_,
          Cp(Utf8("<init>")),
          Cp(Utf8("()V")),
          Seq(Code(_, _, initBytes, _, _))
        ),
        MethodInfo(_,
          Cp(Utf8("main")),
          Cp(Utf8("([Ljava/lang/String;)V")),
          Seq(Code(_, _, mainBytes, _, _))
        )
      ),  // methods
      Seq(SourceFile(Cp(Utf8("HelloWorld.java"))))   // attributes
    ) = classData

    println(initBytes)

    println(mainBytes)
  }
  "java.lang.Object" in {
    val classBytes = getClass.getResourceAsStream("/classpath/java/lang/Object.class")
    val byteArray = new Array[Byte](classBytes.available())
    new DataInputStream(classBytes).readFully(byteArray)
    //Util.printClass(byteArray)
    val classData = ClassFile.read(ByteBuffer.wrap(byteArray))
    implicit val constantPool = classData.constant_pool

    val AccessFlags = (Access.Super | Access.Public)
    val ClassFile(
      0xcafebabe,  // magic
      0,  // minor_version
      51, // major_version
      _,  // constant_pool
      AccessFlags,  // access_flags
      Cp(ClassRef(Cp(Utf8("java/lang/Object")))),  // this_class
      0,  // super_class
      Nil,  // interfaces
      Nil,  // fields
      Seq(
        MethodInfo(Access.Public, Cp(Utf8("<init>")), Cp(Utf8("()V")), Seq(
          Code(_, _, Seq(-79), Nil, Seq(LineNumberTable(Seq(LineNumberData(_, _)))))
        )),
        MethodInfo(Access.Protected, Cp(Utf8("clone")), Cp(Utf8("()Ljava/lang/Object;")),
          Seq(Code(_, _, _, _, _), Exceptions(Seq(Cp(ClassRef(Cp(Utf8("java/lang/CloneNotSupportedException")))))))
        ),
        MethodInfo(_, Cp(Utf8("clone")), Cp(Utf8("(Ljava/lang/Object;)Ljava/lang/Object;")), Nil),
        MethodInfo(Access.Public, Cp(Utf8("equals")), Cp(Utf8("(Ljava/lang/Object;)Z")), _),
        MethodInfo(Access.Protected, Cp(Utf8("finalize")), Cp(Utf8("()V")), _),
        MethodInfo(_, Cp(Utf8("getClass")), Cp(Utf8(_)), _),
        MethodInfo(_, Cp(Utf8("getVMClass")), Cp(Utf8("(Ljava/lang/Object;)Lavian/VMClass;")), _),
        MethodInfo(_, Cp(Utf8("hashCode")), Cp(Utf8("()I")), _),
        MethodInfo(_, Cp(Utf8("notify")), Cp(Utf8("()V")), _),
        MethodInfo(_, Cp(Utf8("notifyAll")), Cp(Utf8("()V")), _),
        MethodInfo(_, Cp(Utf8("toString")), Cp(Utf8("()Ljava/lang/String;")), _),
        MethodInfo(_, Cp(Utf8("wait")), Cp(Utf8("()V")), _),
        MethodInfo(_, Cp(Utf8("wait")), Cp(Utf8("(J)V")), _),
        MethodInfo(_, Cp(Utf8("wait")), Cp(Utf8("(JI)V")), _)
      ),  // methods
      _   // attributes
    ) = classData

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
