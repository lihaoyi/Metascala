package svm

import collection.mutable
import imm._
import imm.Attached.LineNumber
import annotation.tailrec
import java.security.AccessController
import virt.{Obj}
import svm.virt
import virt.Type
object VM{
  val lines = mutable.Buffer.empty[String]
  def log(s: String) = lines.append(s)
  var count = 0
}
import VM._

class VM(val natives: Natives = Natives.default, val printMore: Boolean = false) {

  private[this] implicit val vm = this
  implicit object InternedStrings extends (virt.Obj => virt.Obj){
    val strings = mutable.Map.empty[String, virt.Obj]
    def apply(s: virt.Obj) = {
      val realString = Virtualizer.fromVirtual[String](s)
      if (strings.contains(realString)){
        strings(realString)
      }else{
        strings(realString) = s
        s
      }
    }
  }
  implicit object Types extends (imm.Type => virt.Type){
    val classes = mutable.Map.empty[imm.Type, virt.Type]
    def apply(t: imm.Type): virt.Type = {
      classes.get(t) match{
        case Some(clsObj) => clsObj
        case None =>
          val clsObj = t match{
            case t: imm.Type.Cls => new virt.Cls(t)
            case _ => new virt.Type(t)
          }
          classes(t) = clsObj
          clsObj
      }
    }
  }

  implicit object Classes extends (imm.Type.Cls => svm.Cls){
    val classes = mutable.Map.empty[imm.Type.Cls, svm.Cls]
    def apply(t: imm.Type.Cls): svm.Cls = {

      classes.get(t) match {
        case Some(cls) => cls
        case None =>
          val newCls = new svm.Cls(imm.Cls.parse(natives.fileLoader(t.name.replace(".", "/") + ".class").get))
          classes(t) = newCls
          if (t.name != "sun/misc/Unsafe")
            newCls.method("<clinit>", imm.Type.Desc.read("()V")).foreach( m =>
              threads(0).invoke(imm.Type.Cls(t.name), "<clinit>", imm.Type.Desc.read("()V"), Nil)
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
          imm.Type.Cls(bootClass),
          mainMethod,
          imm.Type.Cls(bootClass).cls
            .classData
            .methods
            .find(x => x.name == mainMethod)
            .map(_.desc)
            .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod)),
          args.map(Virtualizer.toVirtual[Any])
        )
      )
    }catch {case x =>
      lines.takeRight(2000).foreach(println)
      threads(0).dumpStack.foreach(println)
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
  lazy val obj = virt.Obj("java/lang/Thread",
    "name" -> "MyThread".toCharArray,
    "group" -> virt.Obj("java/lang/ThreadGroup"),
    "priority" -> 5
  )

  def frame = threadStack.head

  def swapStack(transform: PartialFunction[List[Any], List[Any]]) = {
    frame.stack = transform(frame.stack)
  }
  def getStackTrace =
    threadStack.map { f =>
      new StackTraceElement(
        f.runningClass.name,
        if (f.method.code != Code()) f.method.name + f.method.desc.unparse + " " + f.method.code.insns(f.pc) else "",
        f.runningClass.classData.misc.sourceFile.getOrElse("[no source]"),
        f.method.code.attachments.flatten.reverse.collectFirst{
          case LineNumber(line, startPc) if startPc < f.pc => line
        }.getOrElse(-1)
      )
    }.toList

  def indent = "\t" * threadStack.filter(_.method.name != "Dummy").length

  def step() = {
    VM.count += 1
    if (false){
      println("SnapShot")
      println("\n")
      println(new Object())
      threadStack.foreach(f =>
        println(f.runningClass.name.padTo(60, ' ') + f.method.name.padTo(30, ' ') + f.pc)
      )
      println("\n")
    }
    val topFrame = threadStack.head

    val node = topFrame.method.code.insns(topFrame.pc)
    if (vm.printMore) println(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack)
    if (vm.printMore) println(indent + "---------------------- " + topFrame.pc + "\t" + node )
    topFrame.pc += 1
    node.op(this)


    //log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack.map(x => if (x == null) null else x.getClass))
  }
  def returnVal(x: Option[Any]) = {
//    log(indent + "Returning from " + threadStack.head.runningClass.name + " " + threadStack.head.method.name)
    threadStack.pop()
    x.foreach(value => threadStack.head.stack = value :: threadStack.head.stack)
  }
  def dumpStack =
    threadStack.map(f =>
      f.runningClass.name.padTo(35, ' ') + (f.method.name + f.method.desc.unparse).padTo(35, ' ') + f.pc.toString.padTo(5, ' ') + (try f.method.code.insns(f.pc-1) catch {case x =>})
    )

  @tailrec final def throwException(ex: virt.Obj, print: Boolean = true): Unit = {

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
            throwException(ex, false)
          case Some(TryCatchBlock(start, end, handler, blockType)) =>
            frame.pc = handler
            frame.stack ::= ex
        }
      case None =>
        ex.apply(imm.Type.Cls("java.lang.Throwable"), "stackTrace")
          .asInstanceOf[Array[virt.Obj]]
          .map(x => "" +
            Virtualizer.fromVirtual(x(imm.Type.Cls("java.lang.StackTraceElement"), "declaringClass")) + " " +
          Virtualizer.fromVirtual(x(imm.Type.Cls("java.lang.StackTraceElement"), "methodName")))
          .foreach(println)

        throw new Exception("Uncaught Exception: " + Virtualizer.fromVirtual(ex(imm.Type.Cls("java.lang.Throwable"), "detailMessage")))
    }
  }
  @tailrec final def prepInvoke(cls: imm.Type, methodName: String, desc: imm.Type.Desc, args: Seq[Any]): Unit = {

    cls match{
      case imm.Type.Arr(_) =>
        val trap = vm.natives.trapped.get("java/lang/Object/"+ methodName, desc).get
        val result = trap(this)(args)

        if (result != ()) threadStack.head.stack ::= result
      case cls@imm.Type.Cls(name) =>
        if (vm.printMore) println(indent + "prepInvoke " + cls.name + "\t" + methodName  + desc.unparse)

        vm.natives.trapped.get(cls.name + "/" + methodName, desc) match{
          case Some(trap) =>
            val result = trap(this)(args)
            if (result != ()) threadStack.head.stack ::= result

          case None =>
            cls.cls.classData.methods.find(x => x.name == methodName && x.desc == desc) match{
              case Some(m) if m.code.insns != Nil=>
                //m.code.insns.zipWithIndex
                //  .foreach(x => VM.log(indent + x._2 + "\t" + x._1))

                val stretchedArgs = args.flatMap {
                  case l: Long => Seq(l, l)
                  case d: Double => Seq(d, d)
                  case x => Seq(x)
                }
                if (vm.printMore) println(indent + "args " + stretchedArgs)
                val startFrame = new Frame(
                  runningClass = cls,
                  method = m,
                  locals = mutable.Seq.tabulate(m.misc.maxLocals)(stretchedArgs.orElse{case x => null}),
                  stack = Nil
                )

                //log(indent + "locals " + startFrame.locals)
                threadStack.push(startFrame)
              case _ =>

                cls.cls.ancestry.tail.headOption match{
                  case Some(x) => prepInvoke(x.tpe, methodName, desc, args)
                  case None => throw new Exception("Can't find method " + cls + " " + methodName + " " + desc.unparse)
                }
            }
        }
    }


  }
  def invoke(cls: imm.Type.Cls, methodName: String, desc: imm.Type.Desc, args: Seq[Any]) = {
    val dummyFrame = new Frame(
      runningClass = cls,
      method = Method(0, "Dummy", imm.Type.Desc.read("()V")),
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
  val runningClass: svm.Cls,
  val method: Method,
  val locals: mutable.Seq[Any] = mutable.Seq.empty,
  var stack: List[Any] = Nil)


