package sm
import collection.mutable
import imm.Attached.LineNumber
import imm.{TryCatchBlock, Method, Code}
import annotation.tailrec

class VmThread(val threadStack: mutable.Stack[Frame] = mutable.Stack())(implicit val vm: VM){
  import vm._
  lazy val obj = virt.Obj("java/lang/Thread",
    "name" -> "MyThread".toCharArray,
    "group" -> virt.Obj("java/lang/ThreadGroup"),
    "priority" -> 5
  )

  def frame = threadStack.head

  def getFramesDump = {
    threadStack.map{ f =>
      FrameDump(f.runningClass.name, f.method.name,
        f.method.code.insns.take(f.pc)
                           .zipWithIndex
                           .map(t => t._2 + "\t" + t._1)
      )
    }
  }
  def getStackTrace =
    threadStack.map { f =>
      new StackTraceElement(
        f.runningClass.name,
        if (f.method.code != Code()) f.method.name + f.method.desc.unparse + " " + f.method.code.insns(f.pc) else "",
        f.runningClass.clsData.misc.sourceFile.getOrElse("[no source]"),
        f.method.code.attachments.flatten.reverse.collectFirst{
          case LineNumber(line, startPc) if startPc < f.pc => line
        }.getOrElse(-1)
      )
    }.toList

  def indent = "\t" * threadStack.filter(_.method.name != "Dummy").length

  def step() = {


    val topFrame = threadStack.head

    val node = topFrame.method.code.insns(topFrame.pc)
    vm.log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack)
    vm.log(indent + "---------------------- " + topFrame.pc + "\t" + node )
    topFrame.pc += 1
    node.op(this)

    //log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack.map(x => if (x == null) null else x.getClass))
  }
  def returnVal(x: Option[Any]) = {
//    log(indent + "Primitives from " + threadStack.head.runningClass.name + " " + threadStack.head.method.name)
    threadStack.pop()
    x.foreach(value => threadStack.head.stack.push(value))
  }
  def dumpStack =
    threadStack.map(f =>
      f.runningClass.name.padTo(35, ' ') + (f.method.name + f.method.desc.unparse).padTo(35, ' ') + f.pc.toString.padTo(5, ' ') + (try f.method.code.insns(f.pc-1) catch {case x =>})
    )

  @tailrec final def throwException(ex: virt.Obj, print: Boolean = true): Unit = {
    println("Throwing")
    ex.magicMembers.get("stackData").getOrElse(
      ex.withMagic("stackData", getFramesDump)
    )
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
            frame.stack.push(ex)
        }
      case None =>
        throw new UncaughtVmException(ex.cls.clsData.tpe.unparse,
                                      Virtualizer.fromVirtual(ex(imm.Type.Cls("java.lang.Throwable"), "detailMessage")),
                                      Nil,
                                      ex.magicMembers("stackData").asInstanceOf[mutable.Seq[FrameDump]])
    }
  }
  @tailrec final def prepInvoke(tpe: imm.Type.Entity, methodName: String, desc: imm.Type.Desc, args: Seq[Any])(implicit originalType: imm.Type.Entity = tpe): Unit = {

    vm.log("prepInvoke " + tpe + " " + methodName + desc.unparse)

    (vm.natives.trapped.get(tpe.name + "/" + methodName, desc), tpe) match{
      case (Some(trap), _) =>
        val result = trap(this)(args)
        if (result != ()) threadStack.head.stack.push(result)

      case (None, tpe: imm.Type.Cls) =>


        vm.log("")
        tpe.cls.clsData.methods.find(x => x.name == methodName && x.desc == desc) match {
          case Some(m) if m.code.insns != Nil=>

            val stretchedArgs = args.flatMap {
              case l: Long => Seq(l, l)
              case d: Double => Seq(d, d)
              case x => Seq(x)
            }
            val array = new Array[Any](m.misc.maxLocals)
            stretchedArgs.copyToArray(array)

            vm.log(indent + "args " + stretchedArgs)
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
                virt.Obj("java/lang/RuntimeException",
                  "detailMessage" -> Virtualizer.toVirtual(s"A Can't find method $originalType $methodName ${desc.unparse}")
                )
              )
            }

        }
      case _ =>
        tpe.parent match{
          case Some(x) => prepInvoke(x, methodName, desc, args)
          case None => throwException(
            virt.Obj("java/lang/RuntimeException",
              "detailMessage" -> Virtualizer.toVirtual(s"B Can't find method $originalType $methodName ${desc.unparse}")
            )
          )
        }
    }


  }
  def invoke(cls: imm.Type.Cls, methodName: String, desc: imm.Type.Desc, args: Seq[Any]) = {
    val dummyFrame = new Frame(
      runningClass = cls,
      method = Method(0, "Dummy", imm.Type.Desc.read("()V")),
      locals = mutable.Seq.empty
    )

    threadStack.push(dummyFrame)
    prepInvoke(cls, methodName, desc, args)

    while(threadStack.head != dummyFrame) step()

    threadStack.pop().stack.headOption.getOrElse(())
  }
}
