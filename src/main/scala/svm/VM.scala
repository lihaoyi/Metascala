package svm

import collection.mutable
import model._
import model.Attached.LineNumber
import annotation.tailrec
import java.security.AccessController

object VM{
  val lines = mutable.Buffer.empty[String] 
  def log(s: String) = lines.append(s)
  var count = 0
}
import VM._

class VM(classLoader: String => Array[Byte]) {
  private[this] implicit val vm = this
  implicit object TpeObjs extends (Type => TpeObj){
    val classes = mutable.Map.empty[Type, TpeObj]
    def apply(t: Type): TpeObj = {
      classes.get(t) match{
        case Some(clsObj) => clsObj
        case None =>
          val clsObj = new TpeObj(t)
          classes(t) = clsObj
          clsObj
      }
    }
  }

  implicit object Classes extends (Type.Cls => Cls){
    val classes = mutable.Map.empty[Type.Cls, Cls]
    def apply(t: Type.Cls): Cls = {

      classes.get(t) match {
        case Some(cls) => cls
        case None =>
          val newCls = new Cls(ClassData.parse(classLoader(t.name)))
          classes(t) = newCls
          if (t.name != "sun/misc/Unsafe")
            newCls.method("<clinit>", Type.Desc.read("()V")).foreach( m =>
              threads(0).invoke(newCls, m, Nil)
            )
          newCls
      }
    }
  }

  lazy val threads = List(new VmThread())

  def invoke(bootClass: String, mainMethod: String, args: Seq[Any]) = {
    try{
      Virtualizer.fromVirtual[Any](
        threads(0).invoke(
          Classes(Type.Cls(bootClass)),
          Classes(Type.Cls(bootClass))
            .classData
            .methods
            .find(x => x.name == mainMethod)
            .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod)),
          args.map(Virtualizer.toVirtual[Any])
        )
      )
    }catch {case x =>
      lines.takeRight(1000).foreach(println)
      throw x
    }
  }
  //invoke("java/lang/System", "initializeSystemClass", Nil)
}

object VmThread{
  def apply()(implicit vmt: VmThread) = vmt
}
class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack())(implicit val vm: VM){
  import vm._
  lazy val obj = svm.Obj("java/lang/Thread",
    "name" -> "MyThread".toCharArray,
    "group" -> svm.Obj("java/lang/ThreadGroup"),
    "priority" -> 5
  )
  val nativeX = Natives.nativeX
  def getStackTrace =
    threadStack.map { f =>
      new StackTraceElement(
        f.runningClass.name,
        "lol" + f.method.name + " " + f.method.code.instructions(f.pc),
        f.runningClass.classData.misc.sourceFile.getOrElse("[no source]"),
        f.method.code.attachments.flatten.reverse.collectFirst{
          case LineNumber(line, startPc) if startPc < f.pc => line
        }.getOrElse(-1)
      )
    }.toList

  def indent = "\t" * threadStack.filter(_.method.name != "Dummy").length
  def step() = {

    VM.count += 1
    if (VM.count % 10000 == 0){
      println("SnapShot")
      println("\n")
      threadStack.foreach(f =>
        println(f.runningClass.name + "\t" + f.method.name + "\t" + f.pc)
      )
      println("\n")
    }
    val topFrame = threadStack.head

    val node = topFrame.method.code.instructions(topFrame.pc)
    log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack)
    log(indent + topFrame.pc + "\t---------------------- " + node )
    topFrame.pc += 1
    node.op(Context(this, vm))


    //log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack.map(x => if (x == null) null else x.getClass))
  }
  def returnVal(x: Option[Any]) = {
    log(indent + "Returning from " + threadStack.head.runningClass.name + " " + threadStack.head.method.name)
    threadStack.pop()
    x.foreach(value => threadStack.head.stack = value :: threadStack.head.stack)
  }
  @tailrec final def throwException(ex: svm.Obj): Unit = {
    threadStack.headOption match{
      case Some(frame)=>
        val handler =
          frame.method.misc.tryCatchBlocks
            .filter(x => x.start <= frame.pc && x.end >= frame.pc)
            .filter(x => !x.blockType.isDefined || ex.cls.checkIsInstanceOf(x.blockType.get))
            .headOption


        handler match{
          case None =>
            threadStack.pop()
            throwException(ex)
          case Some(TryCatchBlock(start, end, handler, blockType)) =>
            frame.pc = handler
            frame.stack ::= ex
        }
      case None =>


        ex.apply(Type.Cls("java.lang.Throwable"), "stackTrace")
          .asInstanceOf[Array[svm.Obj]]
          .map(x => "" +
            Virtualizer.fromVirtual(x(Type.Cls("java.lang.StackTraceElement"), "declaringClass")) + " " +
          Virtualizer.fromVirtual(x(Type.Cls("java.lang.StackTraceElement"), "methodName")))
          .foreach(println)

        throw new Exception("Uncaught Exception: ")
    }
  }
  def prepInvoke(cls: Cls, method: Method, args: Seq[Any]) = {
    log(indent + "prepInvoke " + cls.name + " " + method.name)


    method.code.instructions.zipWithIndex.foreach{case (x, i) => log(indent + i + "\t" + x) }

    (Natives.trapped.get(cls.classData.tpe.name + "/" + method.name, method.desc), method) match{
      case (Some(trap), _) =>
        val result = trap(this)(args)
        val topFrame = threadStack.head
        topFrame.stack = result match{
          case () => topFrame.stack
          case nonUnit => nonUnit :: topFrame.stack
        }
      case (_, m) if m.code != Code.Empty =>
        val stretchedArgs = args.flatMap {
          case l: Long => Seq(l, l)
          case d: Double => Seq(d, d)
          case x => Seq(x)
        }
        val startFrame = new Frame(
          runningClass = cls,
          method = method,
          locals = mutable.Seq.tabulate(method.misc.maxLocals)(stretchedArgs.orElse{case x => null}),
          stack = Nil
        )

        log(indent + "locals " + startFrame.locals)
        threadStack.push(startFrame)
        //log(indent + "Invoking " + method.name)
        //log(indent + "Locals " + startFrame.locals)
      case (_, m) if (m.access | Access.Native) != 0 =>
        val topFrame = threadStack.head
        //log(indent + "Native Method Call!")
        //log(indent + args)
        log(m.desc.unparse)
        val foundMethod =
          cls.ancestry
             .flatMap(c => nativeX.get(c.tpe.name + "/" +  m.name, m.desc))
             .headOption

        val result = foundMethod match {
          case None =>
            threadStack.filter(_.method.name != "Dummy").foreach(f =>
              log(f.runningClass.name.padTo(30, ' ') + f.method.name.padTo(20, ' ') + " " + (f.pc-1) + "\t" + f.method.code.instructions(f.pc-1))
            )
            throw new Exception("Can't find Native Method: " + cls.name + " " + method.name + " " + method.desc)
          case Some(n) => n(this)(args)
        }

        topFrame.stack = result match{
          case () => topFrame.stack
          case nonUnit => nonUnit :: topFrame.stack
        }
      case _ =>
        //log(indent + "Empty Method!")
    }

  }
  def invoke(cls: Cls, method: Method, args: Seq[Any]) = {
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
  val runningClass: Cls,
  val method: Method,
  val locals: mutable.Seq[Any] = mutable.Seq.empty,
  var stack: List[Any] = Nil){

}


