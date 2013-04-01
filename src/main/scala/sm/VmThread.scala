package sm
import collection.mutable
import imm.Attached.LineNumber
import imm.{TryCatchBlock, Method, Code}
import annotation.tailrec
import opcodes.Misc.Return
import opcodes.OpCode
import rt.Cls
import vrt.Cat1
import collection.mutable.ArrayBuffer

class VmThread(val threadStack: mutable.ArrayStack[Frame] = mutable.ArrayStack())(implicit val vm: VM){
  import vm._
  lazy val obj = vrt.Obj("java/lang/Thread",
    "name" -> "MyThread".toCharArray,
    "group" -> vrt.Obj("java/lang/ThreadGroup"),
    "priority" -> 5
  )

  private[this] var i = 0
  def getI = i
  def frame = threadStack.top


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

  def optimize(opcode: OpCode) = {
    
    val insnsList = frame.runningClass.insns(frame.methodIndex)
    insnsList(frame.pc-1) = opcode
    opcode.op(this)
  }

  final def step() = {
    val insnsList = frame.runningClass.insns(frame.methodIndex)
    val node = insnsList(frame.pc)
    vm.log(indent + frame.runningClass.name + "/" + frame.method.name + ": " + frame.stack)
    vm.log(indent + "---------------------- " + frame.pc + "\t" + node )
    frame.pc += 1
    i += 1
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
      case Some(value) => threadStack.top.stack.push(value)
      case None => () //donothing
    }
  }
  def dumpStack =
    threadStack.map(f =>
      f.runningClass.name.padTo(35, ' ') + (f.method.name + f.method.desc.unparse).padTo(35, ' ') + f.pc.toString.padTo(5, ' ') + (try f.method.code.insns(f.pc-1) catch {case x =>})
    )

  @tailrec final def throwException(ex: vrt.Obj, print: Boolean = true): Unit = {


    threadStack.headOption match{
      case Some(frame)=>
        val handler =
          frame.method.misc.tryCatchBlocks
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
          case Some(TryCatchBlock(start, end, handler, blockType)) =>
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


  final def prepInvoke(mRef: rt.MethodRef,
                       args: Seq[vrt.StackVal]) = {
//    println("PrepInvoke " + mRef)
    mRef match{
      case rt.MethodRef.Native(index) =>
        val ((name, desc), op) = vm.natives.trappedIndex(index)
        val result = op(this)(args)
        if(desc.ret != imm.Type.Prim('V'))threadStack.top.stack.push(result.toStackVal)

      case rt.MethodRef.Cls(tpeIndex, methodIndex) =>
        val cls = vm.Classes.clsIndex(tpeIndex)
        val method = cls.clsData.methods(methodIndex)

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
          method = method,
          methodIndex = methodIndex,
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

    while(threadStack.top != dummyFrame) step()

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


