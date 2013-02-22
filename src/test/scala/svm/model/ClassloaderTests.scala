package svm.model

import org.scalatest.FreeSpec
import svm.model.Util.Print
import svm.model.Method
import collection.generic.SeqFactory

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

      import OpCode._
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

  "java.lang.Object" in {

    val classData = ClassFile.parse(svm.Util.loadClass("classpath.java.lang.Object"))
    import OpCode._
    import Attached._
    val ClassFile(33, "java/lang/Object", null, Nil, Nil,
        List(
          Method(1, "<init>", "()V",Nil,Code(List(Return),List(List(LineNumber(13,0)))),_, _),
          cloneMethod,
          Method(266, "clone", "(Ljava/lang/Object;)Ljava/lang/Object;", Nil, Code(Nil, Nil),_, _),
          equalsMethod,
          Method(4, "finalize", "()V", List("java/lang/Throwable"),Code(
            List(Return),
            List(List(LineNumber(28,0)))),
            _, _
          ),
          Method(17, "getClass", "()Ljava/lang/Class;",List(),Code(
            List(
              ALoad(0),
              InvokeStatic("java/lang/Object", "getVMClass", "(Ljava/lang/Object;)Lavian/VMClass;"),
              InvokeStatic("avian/SystemClassLoader", "getClass", "(Lavian/VMClass;)Ljava/lang/Class;"),
              AReturn
            ),List(
              List(LineNumber(31,0)),
              Nil, Nil, Nil
            )
          ),_, _),
          Method(266, "getVMClass", "(Ljava/lang/Object;)Lavian/VMClass;", List(), Code.Empty, _, _),
          Method(257, "hashCode", "()I", Nil, Code.Empty, _, _),
          Method(273, "notify", "()V", Nil, Code.Empty, _, _),
          Method(273, "notifyAll", "()V", Nil, Code.Empty, _, _),
          Method(257, "toString", "()Ljava/lang/String;", Nil, Code.Empty, _, _),
          waitMethod1,
          Method(273, "wait", "(J)V", List("java/lang/InterruptedException"), Code.Empty, _, _),
          waitMethod2
        ), _

    ) = classData
    val Method(4, "clone", "()Ljava/lang/Object;", List("java/lang/CloneNotSupportedException"), Code(
      List(
        ALoad(0),
        InstanceOf("java/lang/Cloneable"),
        IfNe(7),
        ALoad(0),
        InvokeVirtual("java/lang/Object", "getClass", "()Ljava/lang/Class;"),
        InvokeVirtual("java/lang/Class", "isArray", "()Z"),
        IfEq(10),
        ALoad(0),
        InvokeStatic("java/lang/Object", "clone", "(Ljava/lang/Object;)Ljava/lang/Object;"),
        AReturn,
        New("java/lang/CloneNotSupportedException"),
        Dup,
        ALoad(0),
        InvokeVirtual("java/lang/Object", "getClass", "()Ljava/lang/Class;"),
        InvokeVirtual("java/lang/Class", "getName", "()Ljava/lang/String;"),
        InvokeSpecial("java/lang/CloneNotSupportedException", "<init>", "(Ljava/lang/String;)V"),
        AThrow
      ),List(
        List(LineNumber(15,0)),
        Nil, Nil, Nil, Nil, Nil, Nil,
        List(Frame(3,List(),List()), LineNumber(16,7)),
        Nil, Nil,
        List(Frame(3,List(),List()), LineNumber(18,10)),
        Nil, Nil, Nil, Nil, Nil, Nil
      )
    ),_, _) = cloneMethod
/*
    val Method(4, "clone", "()Ljava/lang/Object;", List("java/lang/CloneNotSupportedException"),Code(
      List(
        ALoad(0),
        InstanceOf("java/lang/Cloneable"),
        IfNe(7),
        ALoad(0),
        InvokeVirtual("java/lang/Object", "getClass", "()Ljava/lang/Class;"),
        InvokeVirtual("java/lang/Class", "isArray", "()Z"),
        IfEq(10),
        ALoad(0),
        InvokeStatic("java/lang/Object", "clone", "(Ljava/lang/Object;)Ljava/lang/Object;"),
        AReturn,
        New("java/lang/CloneNotSupportedException"),
        Dup,
        ALoad(0),
        InvokeVirtual("java/lang/Object", "getClass", "()Ljava/lang/Class;"),
        InvokeVirtual("java/lang/Class", "getName", "()Ljava/lang/String;"),
        InvokeSpecial("java/lang/CloneNotSupportedException", "<init>", "(Ljava/lang/String;)V"),
        AThrow
      ),List(
        List(LineNumber(15,0)),
        List(), List(), List(), List(), List(), List(),
        List(Frame(3,List(),List()), LineNumber(16,7)),
        List(), List(),
        List(Frame(3,List(),List()), LineNumber(18,10)),
        List(), List(), List(), List(), List(), List())
    ),_, _) = cloneMethod*/
    val Method(1, "equals", "(Ljava/lang/Object;)Z", Nil,Code(
      List(
        ALoad(0),
        ALoad(1),
        IfACmpNe(5),
        IConst1,
        Goto(6),
        IConst0,
        IReturn
      ),List(
        List(LineNumber(25,0)),
        Nil, Nil, Nil, Nil,
        List(Frame(3, Nil, Nil)),
        List(Frame(4, Nil, List(1)))
      )
    ),_, _) = equalsMethod

    val Method(17, "wait", "()V", List("java/lang/InterruptedException"), Code(
      List(
        ALoad(0),
        LConst0,
        InvokeVirtual("java/lang/Object", "wait", "(J)V"),
        Return
      ),List(
        List(LineNumber(45,0)),
        Nil, Nil,
        List(LineNumber(46,3))
      )
    ),_, _) = waitMethod1

    val Method(17, "wait", "(JI)V", List("java/lang/InterruptedException"), Code(
      List(
        ILoad(3),
        IfEq(6),
        LLoad(1),
        LConst1,
        LAdd,
        LStore(1),
        ALoad(0),
        LLoad(1),
        InvokeVirtual("java/lang/Object", "wait", "(J)V"),
        Return
      ),List(
        List(LineNumber(53,0)),
        List(),
        List(LineNumber(54,2)),
        List(), List(), List(),
        List(Frame(3, Nil, Nil), LineNumber(56,6)),
        List(), List(),
        List(LineNumber(57,9)))),
      _, _) = waitMethod2
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
