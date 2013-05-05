package metascala.rt

import scala.collection.mutable
import annotation.tailrec
import collection.mutable.ArrayBuffer
import metascala._
import imm.Attached.LineNumber
import metascala.imm.Attached.LineNumber
import metascala.opcodes.OpCode
import scala.Some
import metascala.UncaughtVmException
import metascala.vrt
import metascala.imm
import metascala.imm.Access
import metascala.ssa.Insn.{Push, InvokeStatic, ReturnVal}

/**
 * A single thread within the Metascala VM.
 */
class Thread(val threadStack: mutable.ArrayStack[Frame] = mutable.ArrayStack())(implicit val vm: VM){
  import vm._

  private[this] var opCount = 0L
  def getOpCount = opCount
  def frame = threadStack.top

  var returnedVal: Any = 0

  def indent = "\t" * threadStack.filter(_.method.sig.name != "Dummy").length


  final def step() = {
    val insnsList = frame.method.insns
    val node = insnsList(frame.pc)

    frame.method.method.code.attachments(frame.pc).collectFirst{
      case LineNumber(line, _) => frame.lineNum = line
    }

    println(indent + frame.runningClass.name + "/" + frame.method.sig.unparse + ": " + frame.locals.toSeq)
    println(indent + frame.pc + "\t" + node )
    //vm.log(indent + vm.Heap.dump.replace("\n", "\n" + indent))
    frame.pc += 1
    opCount += 1
    node match {
      case ReturnVal(sym) => returnVal(sym.length, sym.lift(0).map(_.n).getOrElse(0))
      case Push(target, value) =>
        value.copyToArray(frame.locals, target)
      case InvokeStatic(target, sources, owner, sig) =>
        resolveDirectRef(owner, sig).map{ m =>

          val args = sources.flatMap(s => frame.locals.slice(s.n, s.n + s.size))

          prepInvoke(m, args)
        }
    }
  }

  def trace = {

    threadStack.map( f =>
      new StackTraceElement(
        f.runningClass.name,
        f.method.method.name,
        f.runningClass.clsData.misc.sourceFile.getOrElse("<unknown file>"),
        f.lineNum
      )
    ).toArray
  }

  def returnVal(size: Int, index: Int) = {

    threadStack.lift(1) match{
      case Some(frame) =>
      case None =>
        println("RETURNING")
        println(frame.locals.toSeq)
        returnedVal = Virtualizer.popVirtual(frame.method.method.desc.ret, reader(frame.locals, index))
        this.threadStack.pop
    }
  }
  final def throwExWithTrace(clsName: String, detailMessage: String) = {
    throwException(
      vrt.Obj.allocate(clsName,
        "stackTrace" -> trace.toVirtObj,
        "detailMessage" -> detailMessage.toVirtObj
      )
    )
  }

  @tailrec final def throwException(ex: vrt.Obj, print: Boolean = true): Unit = {
    vm.log(indent + "THROWING EXCEPTION " + ex.cls.name)
    threadStack.headOption match{
      case Some(frame)=>
        val handler =
          frame.method.method.misc.tryCatchBlocks
            .filter{x =>
            x.start <= frame.pc &&
              x.end >= frame.pc &&
              !x.blockType.isDefined ||
              x.blockType.map(ex.cls.typeAncestry.contains).getOrElse(false)
          }.headOption

        handler match{
          case None =>
            threadStack.pop()
            throwException(ex, false)
          case Some(imm.TryCatchBlock(start, end, handler, blockType)) =>
            frame.pc = handler

        }
      case None =>
        throw new UncaughtVmException(
          ex.address.toRealObj[Throwable]
        )
    }
  }

  final def prepInvoke(mRef: rt.Method,
                       args: Seq[Int]) = {
    println(indent + "PrepInvoke " + mRef + " with " + args)

    mRef match{
      case rt.Method.Native(clsName, imm.Sig(name, desc), op) =>
        //threadStack.headOption.map(f => args.map(f.push))
        //op(this)
      case m @ rt.Method.Cls(cls, methodIndex, method) =>

        assert((m.method.access & Access.Native) == 0, "method cannot be native: " + cls.name + " " + method.sig.unparse)
        println("NEW FRAME " + m.localsSize)
        val startFrame = new Frame(
          runningClass = cls,
          method = m,
          locals = args.toArray.padTo(m.localsSize, 0)
        )

        //log(indent + "locals " + startFrame.locals)
        threadStack.push(startFrame)
    }
  }
  final def prepInvoke(tpe: imm.Type,
                       sig: imm.Sig,
                       args: Seq[Any])
                       : Unit = {

    val tmp = mutable.Buffer.empty[Val]

    args.map(
      Virtualizer.pushVirtual(_, tmp.append(_: Int))
    )

    prepInvoke(
      vm.resolveDirectRef(tpe.cast[imm.Type.Cls], sig).get,
      tmp
    )

  }
  def invoke(mRef: rt.Method, args: Seq[Int]): Any = {
    val startHeight = threadStack.length
    prepInvoke(mRef, args)

    while(threadStack.length != startHeight) step()

    returnedVal
  }

  def invoke(cls: imm.Type.Cls, sig: imm.Sig, args: Seq[Any]): Any = {
    val startHeight = threadStack.length
    prepInvoke(cls, sig, args)

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
            var lineNum: Int = 0,
            val locals: Array[Val])


