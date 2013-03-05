package svm.model

import org.scalatest.FreeSpec

class ClassloaderTests extends FreeSpec{


  "loading class files" - {
    "hello world" in {
      val classData = ClassData.parse(svm.Util.loadClass("svm.helloworld.HelloWorld"))

      val ClassData(
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
      val Method(1, "<init>", TypeDesc(Nil, Type.Primitive("V")), Nil, Code(
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


      val Method(9, "main", _/*"([Ljava/lang/String;)V"*/, Nil, Code(
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
    import Type._
    "hello word" in {
      val TypeDesc(Seq(), Primitive("V")) = TypeDesc.read("()V")
    }
    "more" in {
      val TypeDesc(Seq(Primitive("J"), Primitive("Z")), Primitive("I")) = TypeDesc.read("(JZ)I")
      val TypeDesc(Seq(Primitive("I"), Primitive("F")), Primitive("V")) = TypeDesc.read("(IF)V")
    }
    "more with objects" in {
      val TypeDesc(Seq(Class("java/lang/Object")), Primitive("I")) = TypeDesc.read("(Ljava/lang/Object;)I")
      val TypeDesc(Seq(Primitive("I"), Class("java/lang/String")), Array(Primitive("I"))) = TypeDesc.read("(ILjava/lang/String;)[I")
      val TypeDesc(Seq(Array(Primitive("I"))), Class("java/lang/Object")) = TypeDesc.read("([I)Ljava/lang/Object;")
      val TypeDesc(
        Seq(Primitive("I"), Primitive("D"), Class("java/lang/Thread")),
        Class("java/lang/Object")
      ) = TypeDesc.read("(IDLjava/lang/Thread;)Ljava/lang/Object;")
    }
  }
}
