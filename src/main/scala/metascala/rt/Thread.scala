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

  def swapOpCode(opcode: OpCode) = {
    val insnsList = frame.method.insns
    insnsList(frame.pc-1) = opcode
    opcode.op(this)
  }

  final def step() = {
    val insnsList = frame.method.insns
    val node = insnsList(frame.pc)

//    println(indent + frame.runningClass.name + "/" + frame.method.sig.unparse + ": " + frame.stackDump)
//    println(indent + "---------------------- " + frame.pc + "\t" + node )
//    println(indent + vm.Heap.dump.replace("\n", "\n" + indent))
    frame.pc += 1
    opCount += 1
    node.op(this)

  }

  def trace = {

    threadStack.map( f =>
      new StackTraceElement(
        f.runningClass.name,
        f.method.method.name,
        f.runningClass.clsData.misc.sourceFile.getOrElse("<unknown file>"),
        0
      )
    ).toArray
  }

  def returnVal(n: Int) = {

    threadStack.lift(1) match{
      case Some(frame) =>
        val x = this.popArgs(n)
        this.threadStack.pop
        this.pushFrom(x, 0, n)
      case None =>
        val x = this.popArgs(n)
        returnedVal = Virtualizer.popVirtual(frame.method.method.desc.ret, reader(x, 0))
        this.threadStack.pop
    }
  }

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
          }.headOption

        handler match{
          case None =>
            threadStack.pop()
            throwException(ex, false)
          case Some(imm.TryCatchBlock(start, end, handler, blockType)) =>
            frame.pc = handler
            frame.popAll
            frame.push(ex.address)
        }
      case None =>
        throw new UncaughtVmException(
          Virtualizer.popVirtual(ex.cls.clsData.tpe, () => ex.address).cast[Throwable]
        )
    }
  }




  final def prepInvoke(mRef: rt.Method,
                       args: Seq[Int]) = {
//    println(indent + "PrepInvoke " + mRef + " with " + args)

    mRef match{
      case rt.Method.Native(clsName, imm.Sig(name, desc), op) =>
        threadStack.headOption.map(f => args.map(f.push))
        op(this)
      case m @ rt.Method.Cls(cls, methodIndex, method) =>
        assert((m.method.access & Access.Native) == 0, "method cannot be native: " + cls.name + " " + method.name)

        val startFrame = new Frame(
          runningClass = cls,
          method = m,
        locals = mutable.Seq(args:_*).padTo(m.method.misc.maxLocals, 0)
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
            val locals: mutable.Seq[Val] = mutable.Seq.empty){

  val stack = new Array[Int](method.method.misc.maxStack)
  var index = 0
  def push(n: Int) = {

    stack(index) = n
    index += 1
  }
  def pop = {
    index -= 1
    stack(index)
  }
  def popAll = {
    index = 0
  }
  def stackDump = stack.take(index).toList
}


