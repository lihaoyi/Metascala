package svm

import model.Attribute.Code
import model.ConstantInfo.Utf8Info
import model.opcodes.OpCodeGen.Context
import model.opcodes.OpCodes
import model.{MethodInfo, ClassFile}
import java.nio.ByteBuffer
import collection.mutable

class VirtualMachine(classLoader: String => Array[Byte]){
  val classes = mutable.Map.empty[String, Class]

  val threads = List(new VmThread(classes = getClassFor))
  var heap = Set.empty[Object]

  def getClassFor(name: String): Class = {
    require(!name.contains("."))

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
    val bc = getClassFor(bootClass)

    threads(0).invoke(
      bc,
      getClassFor(bootClass)
        .classFile
        .methods
        .find(x => x.name == mainMethod)
        .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod))
    )
  }
}

class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack(), classes: String => svm.Class){
  def invoke(cls: Class, method: MethodInfo) = {
    val invokeId = util.Random.nextInt(100)
    val code = method.attributes.collect{case x: Code => x: Code}.head

    val dummyFrame = new Frame(
      runningClass = cls,
      method = MethodInfo(0.toShort, "Dummy", "", Nil),
      locals = mutable.Seq(),
      stack = Nil
    )
    val startFrame = new Frame(
      runningClass = cls,
      method = method,
      locals = mutable.Seq.fill(code.max_locals)(null),
      stack = Nil
    )
    threadStack.push(dummyFrame, startFrame)


    while(threadStack.head != dummyFrame){
      val topFrame = threadStack.head
      val bytes = topFrame.method.attributes.collect{case x: Code => x: Code}.head.code
      val opcode = OpCodes(bytes(topFrame.pc))

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
          x.foreach(value => threadStack.head.stack = value :: threadStack.head.stack)
        }
      )
      topFrame.pc += 1
      opcode.op(ctx)
    }
    threadStack.pop().stack.headOption
  }
}

class Frame(
  var pc: Int = 0,
  val runningClass: Class,
  val method: MethodInfo,
  val locals: mutable.Seq[Any] = mutable.Seq(),
  var stack: List[Any] = Nil){

}

