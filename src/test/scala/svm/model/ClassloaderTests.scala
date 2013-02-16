package svm.model

import org.scalatest.FreeSpec
import AttributeInfo.{Code, SourceFile}
import java.nio.ByteBuffer
import ConstantInfo.Utf8
import ConstantInfo.{ClassRef => ConstClass}

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
      Cp(ConstClass(Cp(Utf8("helloworld/HelloWorld")))),  // this_class
      Cp(ConstClass(Cp(Utf8("java/lang/Object")))),  // super_class
      Seq(),  // interfaces
      Seq(),  // fields
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
