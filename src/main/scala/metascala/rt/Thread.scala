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

  def getStackTrace =
    threadStack.map { f =>
      new StackTraceElement(
        f.runningClass.name,
        if (f.method.method.code != imm.Code()) f.method.sig.unparse + " " + f.method.method.code.insns(f.pc) else "",
        f.runningClass.clsData.misc.sourceFile.getOrElse("[no source]"),
        f.method.method.code.attachments.flatten.reverse.collect{
          case LineNumber(line, startPc) if startPc < f.pc => line
        }.headOption.getOrElse(-1)
      )
    }.toList

  def indent = "\t" * threadStack.filter(_.method.sig.name != "Dummy").length

  def swapOpCode(opcode: OpCode) = {
    val insnsList = frame.method.insns
    insnsList(frame.pc-1) = opcode
    vm.log(indent + "SWAPPED")
    vm.log(indent + frame.runningClass.name + "/" + frame.method.sig.name + ": " + frame.stack)
    vm.log(indent + "---------------------- " + frame.pc + "\t" + opcode)
    opcode.op(this)
  }

  final def step() = {
    val insnsList = frame.method.insns
    val node = insnsList(frame.pc)

   // println(indent + frame.runningClass.name + "/" + frame.method.sig.unparse + ": " + frame.stack)
   // println(indent + "---------------------- " + frame.pc + "\t" + node )
    frame.pc += 1
    opCount += 1

    node.op(this)
  }
  def returnVal(x: Int) = {
    println("Returning " + x)

    val oldTop = threadStack.pop()
    threadStack.headOption match{
      case Some(frame) => for (i <- 0 until x) frame.stack.push(oldTop.stack.pop())
      case None => returnedVal = popVirtual(oldTop.method.method.desc.ret, oldTop.stack)
    }
  }

  def popVirtual(tpe: imm.Type, stack: mutable.ArrayStack[Val]) = {
    tpe match {
      case imm.Type.Prim('V') => ()
      case imm.Type.Prim('Z') => Z(stack.pop)
      case imm.Type.Prim('B') => B(stack.pop)
      case imm.Type.Prim('C') => C(stack.pop)
      case imm.Type.Prim('S') => S(stack.pop)
      case imm.Type.Prim('I') => I(stack.pop)
      case imm.Type.Prim('F') => F(stack.pop)
      case imm.Type.Prim('J') =>
        val devirt = J.pop(stack.pop)
        println("DEVIRT " + devirt)
        devirt
      case imm.Type.Prim('D') =>
        val devirt = D.pop(stack.pop)
        println("DEVIRT " + devirt)
        devirt
      case imm.Type.Cls(_) => ???
      case imm.Type.Arr(_) => ???
    }
  }
  def pushVirtual(thing: Any, out: Val => Unit) = {
    thing match {
      case b: Boolean => Z.push(b, out)
      case b: Byte    => B.push(b, out)
      case b: Char    => C.push(b, out)
      case b: Short   => S.push(b, out)
      case b: Int     => I.push(b, out)
      case b: Float   => F.push(b, out)
      case b: Long    => J.push(b, out)

      case b: Double  => D.push(b, out)
      case imm.Type.Cls(_) => ???
      case imm.Type.Arr(_) => ???
    }
  }


  final def prepInvoke(mRef: rt.Method,
                       args: Seq[Int]) = {
    println(indent + "PrepInvoke " + mRef + " with " + args)


    mRef match{
      case rt.Method.Native(clsName, imm.Sig(name, desc), op) =>
        val result = op(this)(args)
        if(desc.ret != imm.Type.Prim('V')) threadStack.top.stack.push(result)

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
    for(arg <- args.reverse){
      this.pushVirtual(arg, {v =>
        tmp.append(v)
      })
    }
    prepInvoke(
      vm.resolveDirectRef(tpe.cast[imm.Type.Cls], sig).get,
      tmp.reverse
    )


  }
  def invoke(mRef: rt.Method, args: Seq[Int]): Any = {
    println(indent + "Invoke A")
    val startHeight = threadStack.length
    prepInvoke(mRef, args)

    while(threadStack.length != startHeight) step()

    returnedVal
  }

  def invoke(cls: imm.Type.Cls, sig: imm.Sig, args: Seq[Any]): Any = {
    println(indent + "Invoke B")
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
            val locals: mutable.Seq[Val] = mutable.Seq.empty,
            val stack: mutable.ArrayStack[Val] = mutable.ArrayStack.empty[Val])


