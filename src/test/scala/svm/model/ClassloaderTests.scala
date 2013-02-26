package svm.model

import org.scalatest.FreeSpec
import svm.model.Util.Print
import svm.model.Method
import collection.generic.SeqFactory

class ClassloaderTests extends FreeSpec{


  "loading class files" - {
    "hello world" in {
      val classData = ClassFile.parse(svm.Util.loadClass("svm.helloworld.HelloWorld"))

      val ClassFile(
        0x21, //access,
        "svm/helloworld/HelloWorld", //name,
        Some("java/lang/Object"), //superName,
        Nil, //interfaces
        Nil, //fields
        List(initMethod, mainMethod), //methods
        _ // misc
      ) = classData

      import opcodes.LoadStore._
      import opcodes.StackManip._
      import opcodes.Misc._
      import Attached._
      val Method(1, "<init>", "()V", Nil, Code(
        List(
          ALoad(0),
          InvokeSpecial("java/lang/Object", "<init>", "()V"),
          Return
        ), List(
          List(LineNumber(3, 0)),
          Nil,
          Nil
        )
      ), _, _) = initMethod


      val Method(9, "main", "([Ljava/lang/String;)V", Nil, Code(
        List(
          GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;"),
          Ldc("Hello World"),
          InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V"),
          Return
        ),List(
          List(LineNumber(5, 0)),
          Nil,
          Nil,
          List(LineNumber(6, 3))
        )
      ), _, _) = mainMethod

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
