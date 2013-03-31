package sm
import collection.mutable
import imm.Attached.LineNumber
import imm.{TryCatchBlock, Method, Code}
import annotation.tailrec
import rt.Cls
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
    val insnsList = topFrame.runningClass.insns(topFrame.methodIndex)
    val node = insnsList(topFrame.pc)
    val optimized = node.opt(vm)
    insnsList(topFrame.pc) = optimized
//    println(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack)
//    println(indent + "---------------------- " + topFrame.pc + "\t" + node )
    topFrame.pc += 1
    i += 1
    //if(i % 10000 == 0) println("i: " + i)
    try{
      optimized.op(this)

    }catch{ case e: Throwable =>
      this.dumpStack.foreach(x => vm log x)
      throw e
    }

    //log(indent + topFrame.runningClass.name + "/" + topFrame.method.name + ": " + topFrame.stack.map(x => if (x == null) null else x.getClass))
  }
  def returnVal(x: Option[vrt.StackVal]) = {
    threadStack.pop()
    x.foreach(value => threadStack.head.stack.push(value))
  }
  def dumpStack =
    threadStack.map(f =>
      f.runningClass.name.padTo(35, ' ') + (f.method.name + f.method.desc.unparse).padTo(35, ' ') + f.pc.toString.padTo(5, ' ') + (try f.method.code.insns(f.pc-1) catch {case x =>})
    )

  @tailrec final def throwException(ex: vrt.Obj, print: Boolean = true): Unit = {

    if(!ex.magicMembers.contains("stackData")){
      ex.withMagic("stackData", getFramesDump)
    }

    threadStack.headOption match{
      case Some(frame)=>
        val handler =
          frame.method.misc.tryCatchBlocks
               .filter{x => x.start <= frame.pc && x.end >= frame.pc && !x.blockType.isDefined || ex.cls.checkIsInstanceOf(x.blockType.get)}
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
                                      ex(imm.Type.Cls("java/lang/Throwable"), "detailMessage").cast[vrt.Obj],
                                      Nil,
                                      ex.magicMembers("stackData").asInstanceOf[mutable.Seq[FrameDump]])
    }
  }


  final def prepInvoke(mRef: rt.MethodRef,
                       args: Seq[vrt.StackVal]) = {
//    println("PrepInvoke " + mRef)
    mRef match{
      case rt.MethodRef.Native(index) =>
        val result = vm.natives.trappedIndex(index)._2(this)(args)
        if (result != ()) threadStack.head.stack.push(result.toStackVal)
      case rt.MethodRef.Cls(tpeIndex, methodIndex) =>
        val cls = vm.Classes.clsIndex(tpeIndex)

        val method = cls.clsData.methods(methodIndex)
//        println(cls.name + " " + method.name + method.desc.unparse)
        val array = new Array[vrt.StackVal](method.misc.maxLocals)
        var i = 0
        for (a <- args){
          array(i) = a
          i += a.size
        }
        val startFrame = new Frame(
          runningClass = cls,
          method = method,
          methodIndex = cls.clsData.methods.indexOf(method),
          locals = array
        )

        //log(indent + "locals " + startFrame.locals)
        threadStack.push(startFrame)
    }
  }
  final def prepInvoke(tpe: imm.Type.Entity,
                       methodName: String,
                       desc: imm.Type.Desc,
                       args: Seq[vrt.StackVal])
                       : Unit = {
    //println("Prep Invoking By Name " + vm.Classes(tpe.cast[imm.Type.Cls]).name + " " + methodName + desc.unparse)
    prepInvoke(
      rt.MethodRef.Cls(
        vm.Classes(tpe.cast[imm.Type.Cls]).index,
        vm.Classes(tpe.cast[imm.Type.Cls])
           .clsData
           .methods
           .indexWhere(m => m.name == methodName && m.desc == desc)
      ),
      args
    )

  }


  def invoke(cls: imm.Type.Cls, methodName: String, desc: imm.Type.Desc, args: Seq[vrt.Val]): vrt.Val = {
    val dummyFrame = new Frame(
      runningClass = cls,
      method = Method(0, "Dummy", imm.Type.Desc.read("()V")),
      methodIndex = 0,
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
            val runningClass: Cls,
            val method: Method,
            val methodIndex: Int,
            val locals: mutable.Seq[vrt.StackVal] = mutable.Seq.empty,
            val stack: mutable.ArrayStack[vrt.StackVal] = mutable.ArrayStack.empty[vrt.StackVal])


