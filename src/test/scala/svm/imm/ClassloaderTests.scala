package svm.imm

import org.scalatest.FreeSpec

class ClassloaderTests extends FreeSpec{


  "loading class files" - {
    "hello world" in {
      val classData = Cls.parse(svm.Util.loadClass("svm.helloworld.HelloWorld"))

      val Cls(
        0x21, //access,
        Type.Cls("svm/helloworld/HelloWorld"), //name,
        Some(Type.Cls("java/lang/Object")), //superType,
        Nil, //interfaces
        Nil, //fields
        List(initMethod, mainMethod), //methods
        _ // misc
      ) = classData

      import opcodes.LoadStore._
      import opcodes.StackManip._
      import opcodes.Misc._
      import Attached._
      val Method(1, "<init>", Type.Desc(Nil, Type.Prim("V")), Nil, Code(
        List(
          ALoad(0),
          InvokeSpecial(Type.Cls("java/lang/Object"), "<init>", Type.Desc(Nil, Type.Prim("V"))),
          Return
        ), List(
          List(LineNumber(3, 0)),
          Nil,
          Nil
        )
      ), _, _) = initMethod


      val Method(9, "main", _/*"([Ljava/lang/String;)V"*/, Nil, Code(
        List(
          GetStatic(Type.Cls("java/lang/System"), "out", Type.Cls("java/io/PrintStream")),
          Ldc("Hello World"),
          InvokeVirtual(Type.Cls("java/io/PrintStream"), "println", Type.Desc(Seq(Type.Cls("java/lang/String")), Type.Prim("V"))),
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


  "Descriptor tests" - {
    import Type._
    "hello word" in {
      val Desc(Seq(), Prim("V")) = Desc.read("()V")
    }
    "more" in {
      val Desc(Seq(Prim("J"), Prim("Z")), Prim("I")) = Desc.read("(JZ)I")
      val Desc(Seq(Prim("I"), Prim("F")), Prim("V")) = Desc.read("(IF)V")
    }
    "more with objects" in {
      val Desc(Seq(Cls("java/lang/Object")), Prim("I")) = Desc.read("(Ljava/lang/Object;)I")
      val Desc(Seq(Prim("I"), Cls("java/lang/String")), Arr(Prim("I"))) = Desc.read("(ILjava/lang/String;)[I")
      val Desc(Seq(Arr(Prim("I"))), Cls("java/lang/Object")) = Desc.read("([I)Ljava/lang/Object;")
      val Desc(
        Seq(Prim("I"), Prim("D"), Cls("java/lang/Thread")),
        Cls("java/lang/Object")
      ) = Desc.read("(IDLjava/lang/Thread;)Ljava/lang/Object;")
    }
  }
}
