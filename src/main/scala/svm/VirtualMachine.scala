package svm

import model.Attribute.Code
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

class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack(), val classes: String => svm.Class){

  def step() = {
    val topFrame = threadStack.head
    val bytes = topFrame.method.code.bytes
    val opcode = OpCodes(bytes(topFrame.pc))

    topFrame.pc += 1
    println("-------------------------------------- "+opcode)
    opcode.op(Context(this))
    println(topFrame.stack)
  }

  def invoke(cls: Class, method: MethodInfo) = {
    println("Invoking " + method.name)
    println(method.code)
    val dummyFrame = new Frame(
      runningClass = cls,
      method = MethodInfo(0.toShort, "Dummy", "", Nil),
      locals = mutable.Seq.empty,
      stack = Nil
    )
    println("MaxLocals " + method.code.maxLocals)
    val startFrame = new Frame(
      runningClass = cls,
      method = method,
      locals = mutable.Seq.fill(method.code.maxLocals)(null),
      stack = Nil
    )

    threadStack.push(dummyFrame, startFrame)

    while(threadStack.head != dummyFrame) step()

    println("Ending " + method.name)
    threadStack.pop().stack.headOption
  }
}

class Frame(
  var pc: Int = 0,
  val runningClass: Class,
  val method: MethodInfo,
  val locals: mutable.Seq[Any] = mutable.Seq.empty,
  var stack: List[Any] = Nil){

}

