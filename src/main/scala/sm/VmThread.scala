package sm
import collection.mutable
import imm.Attached.LineNumber
import imm.{TryCatchBlock, Method, Code}
import annotation.tailrec
import vrt.Cat1

class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack())(implicit val vm: VM){
  import vm._
  lazy val obj = vrt.Obj("java/lang/Thread",
    "name" -> "MyThread".toCharArray,
    "group" -> vrt.Obj("java/lang/ThreadGroup"),
    "priority" -> 5
  )

  private[this] var i = 0
  def getI = i
  def frame = threadStack.head

  def getFramesDump = {

    threadStack.map{ f =>
      FrameDump(
        f.runningClass.name,
        f.method.name,
        f.runningClass.clsData.misc.sourceFile.getOrElse(""),
        f.method.code.attachments.take(f.pc).flatten.collect{ case LineNumber(i, _) => i }.lastOption.getOrElse(0)
      )
    }
  }
  def getStackTrace =
    threadStack.map { f =>
      new StackTraceElement(
        f.runningClass.name,
        if (f.method.code != Code()) f.method.name + f.method.desc.unparse + " " + f.method.code.insns(f.pc) else "",
        f.runningClass.clsData.misc.sourceFile.getOrElse("[no source]"),
        f.method.code.attachments.flatten.reverse.collect{
          case LineNumber(line, startPc) if startPc < f.pc => line
        }.headOption.getOrElse(-1)
      )
    }.toList

  def indent = "\t" * threadStack.filter(_.method.name != "Dummy").length

  final def step() = {
    val topFrame = threadStack.head
    val node = topFrame.method.code.insns(topFrame.pc)
    vm.log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack)
    vm.log(indent + "---------------------- " + topFrame.pc + "\t" + node )
    topFrame.pc += 1
    i += 1
    //if(i % 10000 == 0) println("i: " + i)
    try{
      node.op(this)
    }catch{ case e: Throwable =>
      this.dumpStack.foreach(x => vm log x)
      throw e
    }

    //log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack.map(x => if (x == null) null else x.getClass))
  }
  def returnVal(x: Option[vrt.StackVal]) = {
//    log(indent + "Primitives from " + threadStack.head.runningClass.name + " " + threadStack.head.method.name)
    threadStack.pop()
    x.foreach(value => threadStack.head.stack.push(value))
  }
  def dumpStack =
    threadStack.map(f =>
      f.runningClass.name.padTo(35, ' ') + (f.method.name + f.method.desc.unparse).padTo(35, ' ') + f.pc.toString.padTo(5, ' ') + (try f.method.code.insns(f.pc-1) catch {case x =>})
    )

  @tailrec final def throwException(ex: vrt.Obj, print: Boolean = true): Unit = {
    println("throwException " + ex.cls.name)
    getFramesDump.map(f => println(f.clsName + "/" + f.methodName))
    if(!ex.magicMembers.contains("stackData")){
      ex.withMagic("stackData", getFramesDump)
    }

    threadStack.headOption match{
      case Some(frame)=>
        println("A")
        val handler =
          frame.method.misc.tryCatchBlocks
               .filter{x => x.start <= frame.pc && x.end >= frame.pc && !x.blockType.isDefined || ex.cls.checkIsInstanceOf(x.blockType.get)}
               .headOption

        handler match{
          case None =>
            println("AA")
            threadStack.pop()
            throwException(ex, false)
          case Some(TryCatchBlock(start, end, handler, blockType)) =>
            println("AB")
            frame.pc = handler
            frame.stack.push(ex)
        }
      case None =>
        println("B")
        throw new UncaughtVmException(ex.cls.clsData.tpe.unparse,
                                      ex(imm.Type.Cls("java.lang.Throwable"), "detailMessage").cast[vrt.Obj],
                                      Nil,
                                      ex.magicMembers("stackData").asInstanceOf[mutable.Seq[FrameDump]])
    }
  }

  @tailrec final def prepInvoke(tpe: imm.Type.Entity,
                                methodName: String,
                                desc: imm.Type.Desc,
                                args: Seq[vrt.StackVal])
                               (implicit originalType: imm.Type.Entity = tpe): Unit = {
    vm.log(indent + tpe.name + " " + methodName + desc.unparse)

    (vm.natives.trapped.get(tpe.name + "/" + methodName, desc), tpe) match{
      case (Some(trap), _) =>
        val result = trap(this)(args)
        if (result != ()) threadStack.head.stack.push(result.toStackVal)

      case (None, tpe: imm.Type.Cls) =>
        tpe.cls.clsData.methods.find(x => x.name == methodName && x.desc == desc) match {
          case Some(m) if m.code.insns != Nil=>
            vm.log(
              m.code.insns.zipWithIndex.map{ case (b, i) =>
                indent + i + "\t" + b
              }.mkString("\n")
            )

            val array = new Array[vrt.StackVal](m.misc.maxLocals)
            var i = 0

            for (a <- args){
              array(i) = a
              i += a.size
            }


            vm.log(indent + "args " + array.toSeq)
            val startFrame = new Frame(
              runningClass = tpe.cls,
              method = m,
              locals = array
            )

            //log(indent + "locals " + startFrame.locals)
            threadStack.push(startFrame)
          case _ =>
            tpe.parent match{
              case Some(x) => prepInvoke(x, methodName, desc, args)
              case None => throwException(
                vrt.Obj("java/lang/RuntimeException",
                  "detailMessage" -> s"A Can't find method $originalType $methodName ${desc.unparse}"
                )
              )
            }

        }
      case _ =>
        tpe.parent match{
          case Some(x) => prepInvoke(x, methodName, desc, args)
          case None => throwException(
            vrt.Obj("java/lang/RuntimeException",
              "detailMessage" -> s"B Can't find method $originalType $methodName ${desc.unparse}"
            )
          )
        }
    }


  }
  def invoke(cls: imm.Type.Cls, methodName: String, desc: imm.Type.Desc, args: Seq[vrt.Val]): vrt.Val = {
    val dummyFrame = new Frame(
      runningClass = cls,
      method = Method(0, "Dummy", imm.Type.Desc.read("()V")),
      locals = mutable.Seq.empty
    )

    threadStack.push(dummyFrame)
    prepInvoke(cls, methodName, desc, args.map(_.toStackVal))

    while(threadStack.head != dummyFrame) step()

    val x = threadStack.pop().stack.headOption.getOrElse(vrt.Unit)

    x
  }
}

case class FrameDump(clsName: String,
                     methodName: String,
                     fileName: String,
                     lineNumber: Int)


class Frame(var pc: Int = 0,
            val runningClass: sm.Cls,
            val method: Method,
            val locals: mutable.Seq[vrt.StackVal] = mutable.Seq.empty,
            var stack: mutable.ArrayStack[vrt.StackVal] = mutable.ArrayStack.empty[vrt.StackVal])


