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
              threads(0).invoke(Type.Cls(t.name), "<clinit>", Type.Desc.read("()V"), Nil)
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
          Type.Cls(bootClass),
          mainMethod,
          Type.Cls(bootClass).cls
            .classData
            .methods
            .find(x => x.name == mainMethod)
            .map(_.desc)
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
  def frame = threadStack.head

  def swapStack(transform: PartialFunction[List[Any], List[Any]]) = {
    frame.stack = transform(frame.stack)
  }
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
    node.op(this)


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
  @tailrec final def prepInvoke(cls: Type.Cls, methodName: String, desc: Type.Desc, args: Seq[Any]): Unit = {




    Natives.nativeX.get(cls.classData.tpe.name + "/" + methodName, desc) match{
      case Some(trap) =>
        val result = trap(this)(args)
        val topFrame = threadStack.head
        topFrame.stack = result match{
          case () => topFrame.stack
          case nonUnit => nonUnit :: topFrame.stack
        }
      case None =>
        cls.cls.method(methodName, desc) match{
          case Some(m) =>
            val stretchedArgs = args.flatMap {
              case l: Long => Seq(l, l)
              case d: Double => Seq(d, d)
              case x => Seq(x)
            }
            val startFrame = new Frame(
              runningClass = cls,
              method = m,
              locals = mutable.Seq.tabulate(m.misc.maxLocals)(stretchedArgs.orElse{case x => null}),
              stack = Nil
            )

            log(indent + "locals " + startFrame.locals)
            threadStack.push(startFrame)
          case None =>
            cls.cls.ancestry.tail.headOption match{
              case Some(x) =>
                prepInvoke(x.tpe, methodName, desc, args)
              case None =>
                throw new Exception("Can't find method " + cls + " " + methodName + " " + desc.unparse)
            }
        }
    }

  }
  def invoke(cls: Type.Cls, methodName: String, desc: Type.Desc, args: Seq[Any]) = {
    val dummyFrame = new Frame(
      runningClass = cls,
      method = Method(0, "Dummy", Type.Desc.read("()V")),
      locals = mutable.Seq.empty,
      stack = Nil
    )

    threadStack.push(dummyFrame)
    prepInvoke(cls, methodName, desc, args)

    while(threadStack.head != dummyFrame) step()

    threadStack.pop().stack.headOption.getOrElse(())
  }
}


class Frame(
  var pc: Int = 0,
  val runningClass: Cls,
  val method: Method,
  val locals: mutable.Seq[Any] = mutable.Seq.empty,
  var stack: List[Any] = Nil)


