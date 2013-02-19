package svm

import parsing.Attribute.Code
import parsing.ConstantInfo.Utf8Info
import parsing.opcodes.OpCodeGen.Context
import parsing.opcodes.OpCodes
import parsing.{MethodInfo, ClassFile}
import java.io.DataInputStream
import java.nio.ByteBuffer
import collection.mutable

class VirtualMachine(classLoader: String => Array[Byte]){
  val classes = mutable.Map.empty[String, Class]

  val threads = List(new VmThread(classes = getClassFor))
  var heap = Set.empty[Object]


  def getClassFor(name: String): Class = {
    classes.get(name) match{
      case Some(cls) => cls
      case None =>
        classes(name) = loadClass(classLoader(name))
        run(name, "<clinit>")
        classes(name)
    }
  }

  def loadClass(bytes: Array[Byte]) = {
    val classData = ClassFile.read(ByteBuffer.wrap(bytes))
    new Class(classData)
  }

  def run(bootClass: String, mainMethod: String) = {
    val bc = classes(bootClass)
    val cp = bc.classFile.constant_pool
    //println("Looking for: " + mainMethod)

    threads(0).run(
      bc,
      classes(bootClass)
        .classFile
        .methods
        .find(x => x.name == mainMethod)
        .get
    )
  }
}

class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack(), classes: String => svm.Class){
  def run(bootClass: Class, mainMethod: MethodInfo) = {
    val startFrame = new Frame(
      runningClass = bootClass,
      method = mainMethod,
      locals = mutable.Seq(),
      stack = Nil
    )

    threadStack.push(startFrame)
    //println("Main Loop Starting")
    var result: Any = null
    while(threadStack.length != 0){
      val topFrame = threadStack.head
      val bytes = topFrame.method.attributes.collect{case x: Code => x: Code}.head.code
      //println(bytes)
      val ctx = Context(
        this,
        topFrame,
        topFrame.runningClass.classFile.constant_pool,
        topFrame.stack,
        classes,
        {() =>
          val result = bytes(topFrame.pc)
          topFrame.pc += 1
          result
        },
        { x =>
          threadStack.pop()
          (x, threadStack.headOption) match {
            case (Some(value), Some(top)) => top.stack = value :: top.stack
            case (None, Some(top)) =>
            case (either, single) => result = either
          }
        }
      )

      val opcode = OpCodes(bytes(topFrame.pc))
      //println(opcode)
      //println(ctx.stack)
      topFrame.pc = topFrame.pc + 1
      opcode.op(ctx)
      //println("____")
    }
    result
  }
}

class Frame(
  var pc: Int = 0,
  val runningClass: Class,
  val method: MethodInfo,
  val locals: mutable.Seq[Any] = mutable.Seq(),
  var stack: List[Any] = Nil){

}

