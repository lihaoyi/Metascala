package sm.rt

import collection.mutable
import annotation.tailrec
import collection.mutable.ArrayBuffer
import sm._
import imm.Attached.LineNumber
import sm.imm.Attached.LineNumber
import sm.opcodes.OpCode
import scala.Some
import sm.UncaughtVmException
import sm.vrt
import sm.imm

/**
 * A single thread within the ScalaMachine VM.
 */
class Thread(val threadStack: mutable.ArrayStack[Frame] = mutable.ArrayStack())(implicit val vm: VM){
  import vm._
  lazy val obj = vrt.Obj("java/lang/Thread",
    "name" -> "MyThread".toCharArray,
    "group" -> vrt.Obj("java/lang/ThreadGroup"),
    "priority" -> 5
  )


  private[this] var opCount = 0
  def getOpCount = opCount
  def frame = threadStack.top

  var returnedVal: vrt.StackVal = ()

  def getStackTrace =
    threadStack.map { f =>
      new StackTraceElement(
        f.runningClass.name,
        if (f.method.method.code != imm.Code()) f.method.name + f.method.desc.unparse + " " + f.method.method.code.insns(f.pc) else "",
        f.runningClass.clsData.misc.sourceFile.getOrElse("[no source]"),
        f.method.method.code.attachments.flatten.reverse.collect{
          case LineNumber(line, startPc) if startPc < f.pc => line
        }.headOption.getOrElse(-1)
      )
    }.toList

  def indent = "\t" * threadStack.filter(_.method.name != "Dummy").length

  def swapOpCode(opcode: OpCode) = {
    val insnsList = frame.method.insns
    insnsList(frame.pc-1) = opcode
    vm.log(indent + "SWAPPED")
    vm.log(indent + frame.runningClass.name + "/" + frame.method.name + ": " + frame.stack)
    vm.log(indent + "---------------------- " + frame.pc + "\t" + opcode)
    opcode.op(this)
  }

  final def step() = {
    val insnsList = frame.method.insns
    val node = insnsList(frame.pc)
    vm.log(indent + frame.runningClass.name + "/" + frame.method.name + ": " + frame.stack)
    vm.log(indent + "---------------------- " + frame.pc + "\t" + node )
    frame.pc += 1
    opCount += 1
    try{
      node.op(this)
    }catch{ case e: Throwable =>
      this.dumpStack.foreach(x => vm log x)
      throw e
    }

    //log(indent + frame.runningClass.name + "/" + frame.method.name + ": " + frame.stack.map(x => if (x == null) null else x.getClass))
  }
  def returnVal(x: Option[vrt.StackVal]) = {
    threadStack.pop()
    x match{
      case Some(value) => threadStack.headOption match{
        case Some(frame) => frame.stack.push(value)
        case None => returnedVal = value
      }
      case None => () //donothing
    }
  }
  def dumpStack =
    threadStack.map(f =>
      f.runningClass.name.padTo(35, ' ') + (f.method.name + f.method.desc.unparse).padTo(35, ' ') + f.pc.toString.padTo(5, ' ') + (try f.method.method.code.insns(f.pc-1) catch {case x =>})
    )

  @tailrec final def throwException(ex: vrt.Obj, print: Boolean = true): Unit = {

    threadStack.headOption match{
      case Some(frame)=>
        val handler =
          frame.method.method.misc.tryCatchBlocks
               .filter{x =>
                  x.start <= frame.pc &&
                  x.end >= frame.pc &&
                  !x.blockType.isDefined ||
                  ex.cls.typeAncestry.contains(x.blockType.get)
                }
               .headOption

        handler match{
          case None =>
            threadStack.pop()
            throwException(ex, false)
          case Some(imm.TryCatchBlock(start, end, handler, blockType)) =>
            frame.pc = handler
            frame.stack.push(ex)
        }
      case None =>
        throw new UncaughtVmException(ex.cls.clsData.tpe.unparse,
                                      ex(imm.Type.Cls("java/lang/Throwable"), "detailMessage").cast[vrt.Obj],
                                      Nil,
                                      Nil)
    }
  }


  final def prepInvoke(mRef: rt.Method,
                       args: Seq[vrt.StackVal]) = {
    vm.log("PrepInvoke " + mRef.name)
    mRef match{
      case rt.Method.Native((name, desc), op) =>
        val result = op(this)(args)
        if(desc.ret != imm.Type.Prim('V'))threadStack.top.stack.push(result.toStackVal)

      case m @ rt.Method.Cls(tpeIndex, methodIndex, method) =>
        val cls = vm.ClsTable.clsIndex(tpeIndex)

        val array = new Array[vrt.StackVal](method.misc.maxLocals)
        var i = 0
        var j = 0
        while(i < args.length){
          val a = args(i)
          array(j) = a
          j += a.size
          i+= 1
        }

        val startFrame = new Frame(
          runningClass = cls,
          method = m,
          locals = array
        )

        //log(indent + "locals " + startFrame.locals)
        threadStack.push(startFrame)
    }
  }
  final def prepInvoke(tpe: imm.Type,
                       methodName: String,
                       desc: imm.Desc,
                       args: Seq[vrt.StackVal])
                       : Unit = {
    //println("Prep Invoking By Name " + vm.ClsTable(tpe.cast[imm.Type.Cls]).name + " " + methodName + desc.unparse)


    prepInvoke(
      vm.ClsTable(tpe.cast[imm.Type.Cls])
        .methods
        .find(m => m.method.name == methodName && m.method.desc == desc)
        .get,
      args
    )
  }

  def invoke(cls: imm.Type.Cls, methodName: String, desc: imm.Desc, args: Seq[vrt.Val]): vrt.Val = {

    val startHeight = threadStack.length
    prepInvoke(cls, methodName, desc, args.map(_.toStackVal))

    while(threadStack.length != startHeight) step()

    returnedVal
  }
}

case class FrameDump(clsName: String,
                     methodName: String,
                     fileName: String,
                     lineNumber: Int)


/**
 * The stack frame created by every method call
 */
class Frame(var pc: Int = 0,
            val runningClass: rt.Cls,
            val method: rt.Method.Cls,
            val locals: mutable.Seq[vrt.StackVal] = mutable.Seq.empty,
            val stack: mutable.ArrayStack[vrt.StackVal] = mutable.ArrayStack.empty[vrt.StackVal])


