package svm.model

import org.scalatest.FreeSpec
import svm.model.Node._
import svm.model.Node.LineNumber
import svm.model.Node.MethodInsn
import svm.model.Node.VarInsn
import svm.model.Node.Insn
import svm.model.Util.Print

class ClassloaderTests extends FreeSpec{


  "loading class files" - {
    "hello world" in {
      val classData = ClassFile.parse(svm.Util.loadClass("helloworld.HelloWorld"))

      val ClassFile(
        0x21, //access,
        "helloworld/HelloWorld", //name,
        "java/lang/Object", //superName,
        Nil, //interfaces
        Nil, //fields
        List(
          Method(1, "<init>", "()V", Nil ,List(
            Label(),
            LineNumber(3, _),
            VarInsn(25,0),
            MethodInsn(183, "java/lang/Object", "<init>", "()V"),
            Insn(177),
            Label()), _, _
          ),
          Method(9, "main", "([Ljava/lang/String;)V", Nil, List(
            Label(),
            LineNumber(5, _),
            FieldInsn(178, "java/lang/System", "out", "Ljava/io/PrintStream;"),
            LdcInsn(18, "Hello World"),
            MethodInsn(182, "java/io/PrintStream", "println", "(Ljava/lang/String;)V"),
            Label(),
            LineNumber(6, _),
            Insn(177),
            Label()), _, _
          )
        ), //methods
        _ // misc
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
