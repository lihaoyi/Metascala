package svm

import collection.mutable
import model._


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
        classes(name).method("<clinit>").foreach( m =>
          threads(0).invoke(classes(name), m, Nil)
        )
        classes(name)
    }
  }

  def loadClass(bytes: Array[Byte]) = {
    val classData = ClassFile.parse(bytes)
    new Class(classData)
  }

  def invoke(bootClass: String, mainMethod: String, args: Seq[Any]) = {
    val bc = getClassFor(bootClass)

    threads(0).invoke(
      bc,
      getClassFor(bootClass)
        .classFile
        .methods
        .find(x => x.name == mainMethod)
        .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod)),
      args
    )
  }
}

class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack(), val classes: String => svm.Class){

  def step() = {
    val topFrame = threadStack.head

    val node = topFrame.method.code.instructions(topFrame.pc)


  //  println(topFrame.pc + "\t---------------------- " + node )
    topFrame.pc += 1
    node.op(Context(this))
    //println(topFrame.stack)
  }

  def invoke(cls: Class, method: Method, args: Seq[Any]) = {
    println("Invoking " + method.name)
    method.code.instructions.zipWithIndex.foreach{case (x, i) => println(i + "\t" + x) }
    println()

    val dummyFrame = new Frame(
      runningClass = cls,
      method = method.copy(name = "Dummy"),
      locals = mutable.Seq.empty,
      stack = Nil
    )
    println("MaxLocals " + method.misc.maxLocals)
    val startFrame = new Frame(
      runningClass = cls,
      method = method,
      locals = mutable.Seq.fill(method.misc.maxLocals + 1)(null), // +1 in case the last guy is a double
      stack = Nil
    )
    val stretchedArgs = args.flatMap {
      case l: Long => Seq(l, null)
      case d: Double => Seq(d, null)
      case x => Seq(x)
    }
    for (i <- 0 until stretchedArgs.length){
      startFrame.locals(i) = stretchedArgs(i)
    }
    threadStack.push(dummyFrame, startFrame)

    while(threadStack.head != dummyFrame) step()

    println("Ending " + method.name)
    threadStack.pop().stack.headOption.getOrElse(())
  }
}

class Frame(
  var pc: Int = 0,
  val runningClass: Class,
  val method: Method,
  val locals: mutable.Seq[Any] = mutable.Seq.empty,
  var stack: List[Any] = Nil){

}


