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
        classes(name).method("<clinit>", "()V").foreach( m =>
          threads(0).invoke(classes(name), m, Nil)
        )
        classes(name)
    }
  }

  def loadClass(bytes: Array[Byte]) = {
    val classData = ClassFile.parse(bytes)
    new Class(classData, getClassFor)
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

  def indent = "\t" * threadStack.filter(_.method.name != "Dummy").length
  def step() = {
    val topFrame = threadStack.head

    val node = topFrame.method.code.instructions(topFrame.pc)


    //println(indent + topFrame.pc + "\t---------------------- " + node )
    topFrame.pc += 1
    node.op(Context(this))
    //println(indent + topFrame.stack)
  }
  def returnVal(x: Option[Any]) = {
    //println(indent + "Returning!")
    threadStack.pop()
    x.foreach(value => threadStack.head.stack = value :: threadStack.head.stack)
  }
  def prepInvoke(cls: Class, method: Method, args: Seq[Any]) = {
    //println("prepInvoke " + cls.name + " " + method.name)
    //println(method.code)
    if (method.code != Code.Empty){


      val startFrame = new Frame(
        runningClass = cls,
        method = method,
        locals = mutable.Seq.fill(method.misc.maxLocals + 10)(null), // +1 in case the last guy is a double
        stack = Nil
      )
      val stretchedArgs = args.flatMap {
        case l: Long => Seq(l, l)
        case d: Double => Seq(d, d)
        case x => Seq(x)
      }

      for (i <- 0 until stretchedArgs.length){
        startFrame.locals(i) = stretchedArgs(i)
      }

      threadStack.push(startFrame)
      //println(indent + "Invoking " + method.name)
      //println(indent + "Locals " + startFrame.locals)
      //method.code.instructions.zipWithIndex.foreach{case (x, i) => println(indent + i + "\t" + x) }
    }else{
      println("Empty Method!")
    }
  }
  def invoke(cls: Class, method: Method, args: Seq[Any]) = {
    val dummyFrame = new Frame(
      runningClass = cls,
      method = method.copy(name = "Dummy"),
      locals = mutable.Seq.empty,
      stack = Nil
    )
    threadStack.push(dummyFrame)
    prepInvoke(cls, method, args)

    while(threadStack.head != dummyFrame) step()

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


