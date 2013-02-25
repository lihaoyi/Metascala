package svm

import collection.mutable
import model._

object Route{
  def apply(a: (String, Route)*) = Node(a.toMap)
}
trait Route{
  def lookup(s: String): Option[Any]
}
case class Node(children: Map[String, Route]) extends Route{
  def lookup(s: String) = {
    val Array(first, rest) =
      if (s.indexOf('/') != -1 && s.indexOf('/') < s.indexOf('(')){
        s.split("/", 2)
      }else{
        Array(s, "")
      }
    children.get(first).flatMap(_.lookup(rest))
  }
}

case class Leaf(f: Any) extends Route{
  def lookup(s: String) = {
    if (s == "") Some(f)
    else None
  }
}

class VirtualMachine(classLoader: String => Array[Byte]){
  val classes = mutable.Map.empty[String, Class]

  implicit class pimpedMap(s: String){
    def /(a: (String, Route)*) = s -> Node(a.toMap)
    def -(a: Any) = s -> Leaf(a)
  }
  val noOp = () => ()
  val primitiveMap = Map(
    "float" -> "java/lang/Float",
    "int" -> "java/lang/Integer",
    "double" -> "java/lang/Double"
  )
  val nativeX = Route(
    "java"/(
      "lang"/(
        "Class"/(
          "registerNatives()V"-noOp,
          "getPrimitiveClass(Ljava/lang/String;)Ljava/lang/Class;"-((s: svm.Object) => (new Object(classes(primitiveMap(Object.fromVirtual(s).asInstanceOf[String])), getClassFor))),
          "getClassLoader0()Ljava/lang/ClassLoader;"-(() => null),
          "desiredAssertionStatus0(Ljava/lang/Class;)Z"-((x: Any) => 0)
        ),
        "Double"/(
          "doubleToRawLongBits(D)J"-{ java.lang.Double.doubleToRawLongBits(_: Double)}
        ),
        "Float"/(
          "intBitsToFloat(I)F"-{ java.lang.Float.intBitsToFloat(_: Int)},
          "floatToRawIntBits(F)I"-{ java.lang.Float.floatToIntBits(_: Float)}
        ),
        "Object"/(
          "registerNatives()V"-noOp
        ),
        "System"/(
          "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V"-((src: Any, srcPos: Int, dest: Any, destPos: Int, length: Int) =>
            System.arraycopy(src, srcPos, dest, destPos, length)
          ),
          "currentTimeMillis()J"-(() => System.currentTimeMillis()),
          "nanoTime()J"-(() => System.nanoTime()),
          "identityHashCode(Ljava/lang/Object;)I"-((x: svm.Object) => System.identityHashCode(x)),
          "registerNatives()V"-noOp
        )
      )
    )
  )

  val threads = List(new VmThread(classes = getClassFor, nativeX = nativeX))
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
    Object.fromVirtual[Any](
      threads(0).invoke(
        bc,
        getClassFor(bootClass)
          .classFile
          .methods
          .find(x => x.name == mainMethod)
          .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod)),
        args
      )
    )
  }
}

class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack(), val classes: String => svm.Class, nativeX: Route){

  def indent = "\t" * threadStack.filter(_.method.name != "Dummy").length
  def step() = {
    val topFrame = threadStack.head

    val node = topFrame.method.code.instructions(topFrame.pc)


    //println(indent + topFrame.pc + "\t---------------------- " + node )
    topFrame.pc += 1
    node.op(Context(this))

    //println(indent + topFrame.method.name + ": " + topFrame.stack)


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
    }else if ((method.access | Access.Native) != 0){
      val topFrame = threadStack.head
      println("Native Method Call!")
      println(args)
      val result = nativeX.lookup(cls.name + "/" + method.name + method.desc) match{
        case None => throw new Exception("Can't find Native Method: " + cls.name + " " + method.name + " " + method.desc)
        case Some(f: Function0[Any]) => f()
        case Some(f: Function1[Any, Any]) => f(args(0))
        case Some(f: Function2[Any, Any, Any]) => f(args(0), args(1))
        case Some(f: Function3[Any, Any, Any, Any]) => f(args(0), args(1), args(2))
        case Some(f: Function4[Any, Any, Any, Any, Any]) => f(args(0), args(1), args(2), args(3))
        case Some(f: Function5[Any, Any, Any, Any, Any, Any]) => f(args(0), args(1), args(2), args(3), args(4))
      }

      topFrame.stack = result match{
        case () => topFrame.stack
        case nonUnit => nonUnit :: topFrame.stack
      }

    }else {
      println(indent + "Empty Method!")
    }
  }
  def invoke(cls: Class, method: Method, args: Seq[Any]) = {
    val dummyFrame = new Frame(
      runningClass = cls,
      method = method.copy(name = "Dummy"),
      locals = mutable.Seq.empty,
      stack = Nil
    )
    println(indent + "Steppin'")
    threadStack.push(dummyFrame)
    prepInvoke(cls, method, args)

    while(threadStack.head != dummyFrame) step()
    println(indent + "Don' Steppin'")
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


