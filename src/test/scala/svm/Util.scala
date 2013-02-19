package svm

import java.io.DataInputStream

object Util {
  def loadClass(name: String) = {
    val stream = new DataInputStream(
      getClass.getResourceAsStream(s"/$name.class")
    )
    val bytes = new Array[Byte](stream.available())

    stream.readFully(bytes)
    model.Util.printClass(bytes)
    bytes
  }
  class SingleClassVirtualMachine(className: String, val classLoader: String => Array[Byte])
  extends VirtualMachine(classLoader){
    def run(main: String): Any = run(className.replace('.', '/'), main)
  }


}


/*
 L0
    LINENUMBER 8 L0
    ICONST_0
    ISTORE 0
   L1
    LINENUMBER 9 L1
    ICONST_0
    ISTORE 1
   L2
   FRAME APPEND [I I]
    ILOAD 1
    GETSTATIC helloworld/controlflow/Loops.a : I
    IF_ICMPLE L3
    IINC 0 1
    IINC 1 1
    GOTO L2
   L3
    LINENUMBER 10 L3
   FRAME CHOP 1
    ILOAD 0
    IRETURN
   L4
    LOCALVARIABLE i I L2 L3 1
    LOCALVARIABLE c I L1 L4 0
    MAXSTACK = 2
    MAXLOCALS = 2
*/