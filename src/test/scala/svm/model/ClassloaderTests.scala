package svm.model

import org.scalatest.FreeSpec

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
        List(initMethod, mainMethod), //methods
        _ // misc
      ) = classData


      val Method(1, "<init>", "()V", Nil , /*List(
        Label(),
        LineNumber(3, _),
        VarInsn(25,0),
        MethodInsn(183, "java/lang/Object", "<init>", "()V"),
        Insn(177),
        Label())*/_, _, _
        ) = initMethod

      val Method(9, "main", "([Ljava/lang/String;)V", Nil, /*List(
        Label(),
        LineNumber(5, _),
        FieldInsn(178, "java/lang/System", "out", "Ljava/io/PrintStream;"),
        LdcInsn(18, "Hello World"),
        MethodInsn(182, "java/io/PrintStream", "println", "(Ljava/lang/String;)V"),
        Label(),
        LineNumber(6, _),
        Insn(177),
        Label())*/_ , _, _
      ) = mainMethod



      }
    }

    /*"java.lang.Object" in {
      val classData = ClassFile.parse(svm.Util.loadClass("classpath.java.lang.Object"))
      val ClassFile(
        33, "java/lang/Object", null, Nil, Nil,List(
          Method(1, "<init>", "()V", Nil, List(
            Label(),
            LineNumber(13, _),
            Insn(177)
          ), _,_),
          Method(4,"clone","()Ljava/lang/Object;",List("java/lang/CloneNotSupportedException"),_, _,_),
          Method(266, "clone", "(Ljava/lang/Object;)Ljava/lang/Object;", Nil, Nil, _, _),
          Method(1, "equals", "(Ljava/lang/Object;)Z", Nil, List(
            Label(),
            LineNumber(25,_),
            VarInsn(25,0),
            VarInsn(25,1),
            JumpInsn(166, _),
            Insn(4),
            JumpInsn(167, _),
            Label(), Insn(3),
            Label(),
            Insn(172)), _,_),
          Method(4, "finalize", "()V", List("java/lang/Throwable"),List(
            Label(),
            LineNumber(28, _),
            Insn(177)),_, _),
          Method(17, "getClass", "()Ljava/lang/Class;", Nil,List(
            Label(),
            LineNumber(31, _),
            VarInsn(25,0),
            MethodInsn(184, "java/lang/Object", "getVMClass", "(Ljava/lang/Object;)Lavian/VMClass;"),
            MethodInsn(184, "avian/SystemClassLoader", "getClass", "(Lavian/VMClass;)Ljava/lang/Class;"),
            Insn(176)),_, _),
          Method(266, "getVMClass", "(Ljava/lang/Object;)Lavian/VMClass;", List(),List(),_, _),
          Method(257, "hashCode", "()I", List(),List(),_, _),
          Method(273, "notify", "()V", List(),List(),_, _),
          Method(273, "notifyAll", "()V", List(),List(),_, _),
          Method(257, "toString", "()Ljava/lang/String;",List(),List(),_, _),
          Method(17, "wait", "()V", List("java/lang/InterruptedException"),List(
            Label(),
            LineNumber(45, _),
            VarInsn(25,0),
            Insn(9),
            MethodInsn(182,"java/lang/Object","wait","(J)V"),
            Label(),
            LineNumber(46, _),
            Insn(177)),_, _),
          Method(273,"wait", "(J)V", List("java/lang/InterruptedException"),List(),_, _),
          Method(17,"wait","(JI)V",List("java/lang/InterruptedException"),List(
            Label(),
            LineNumber(53, _),
            VarInsn(21,3),
            JumpInsn(153, _),
            Label(),
            LineNumber(54, _),
            VarInsn(22,1),
            Insn(10),
            Insn(97),
            VarInsn(55,1),
            Label(),
            LineNumber(56, _),
            VarInsn(25,0),
            VarInsn(22,1),
            MethodInsn(182, "java/lang/Object", "wait", "(J)V"),
            Label(),
            LineNumber(57, _),
            Insn(177)),_,_)),
          _
        ) = classData




  }*/
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
