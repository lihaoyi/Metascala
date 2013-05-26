package metascala.rt

import scala.collection.mutable
import annotation.tailrec
import collection.mutable.ArrayBuffer
import metascala._
import imm.Attached.LineNumber
import metascala.imm.Attached.LineNumber
import metascala.StackOps.OpCode
import scala.Some
import metascala.UncaughtVmException
import metascala.vrt
import metascala.imm
import metascala.imm.Access
import metascala.opcodes.Insn
import Insn._
import Insn.Ldc
import metascala.imm.Attached.LineNumber
import Insn.Push
import Insn.InvokeStatic
import scala.Some
import metascala.UncaughtVmException
import Insn.ReturnVal

/**
 * A single thread within the Metascala VM.
 */
class Thread(val threadStack: mutable.ArrayStack[Frame] = mutable.ArrayStack())(implicit val vm: VM){
  import vm._

  private[this] var opCount = 0L
  def getOpCount = opCount
  def frame = threadStack.top

  val returnedVal = Array(0, 0)

  def indent = "\t" * threadStack.filter(_.method.sig.name != "Dummy").length


  final def step(): Unit = try {
    val insnsList = frame.method.insns
    val node = insnsList(frame.pc)
    if (frame.runningClass.name.contains("metascala/features/controlflow/Switches")){
      println(indent + "::\t" + frame.runningClass.name + "/" + frame.method.sig.unparse + ": " + frame.locals.toSeq)
      println(indent + "::\t" + frame.pc + "\t" + node )
    }
//    println(indent + "::\t" + vm.Heap.dump.replace("\n", "\n" + indent + "::\t"))
    frame.pc += 1
    opCount += 1

    node match {
      case ReturnVal(sym) =>

        returnVal(frame.method.method.sig.desc.ret.size, sym)
      case Push(prim, target, value) =>
        prim.write(value, writer(frame.locals, target))
      case New(target, cls) =>
        val obj = vrt.Obj.allocate(cls.name)
        frame.locals(target) = obj.address

      case InvokeStatic(target, sources, owner, sig) =>
        resolveDirectRef(owner, sig).map{ m =>
          val args = sources.zip(sig.desc.args)
                            .flatMap{case (s, t) =>
            frame.locals.slice(s, s + t.size)
          }
          prepInvoke(m, args, writer(frame.locals, target))
        }

      case InvokeSpecial(target, sources, owner, sig) =>
        resolveDirectRef(owner, sig).map{ m =>
          val args = sources.zip(1 +: sig.desc.args.map(_.size))
            .flatMap{case (s, t) =>
            frame.locals.slice(s, s + t)
          }
          if(args(0) == 0) throwExWithTrace("java/lang/NullPointerException", "null")
          else prepInvoke(m, args, writer(frame.locals, target))
        }
      case InvokeVirtual(target, sources, owner, sig) =>
        val args = sources.zip(1 +: sig.desc.args.map(_.size))
          .flatMap{case (s, t) =>
          frame.locals.slice(s, s + t)
        }
        ///println("INVOKE VIRTUAL " + args)

        if(args(0) == 0) throwExWithTrace("java/lang/NullPointerException", "null")
        else {
          //println(args(0).obj.cls.clsData.tpe)
          val mRef = args(0).obj.cls.vTableMap(sig)
          prepInvoke(mRef, args, writer(frame.locals, target))
        }

      case ArrayLength(src, dest) =>
        frame.locals(dest) = frame.locals(src).arr.length
      case NewArray(src, dest, typeRef) =>
        val newArray = vrt.Arr.allocate(typeRef, frame.locals(src))
        frame.locals(dest) = newArray.address
      case StoreArray(src, index, array, prim) =>
        val arr = frame.locals(array).arr
        blit(frame.locals, src, arr, frame.locals(index) * prim.size, prim.size)
      case LoadArray(dest, index, array, prim) =>
        val arr = frame.locals(array).arr

        blit(arr, frame.locals(index) * prim.size, frame.locals, dest, prim.size)
      case Ldc(target, thing) =>
        val w = writer(frame.locals, target)
        thing match{
          case s: String =>
            val top = Virtualizer.pushVirtual(s).apply(0)
            frame.locals(target) = top
          case t: org.objectweb.asm.Type =>
            val clsObj = vrt.Obj.allocate("java/lang/Class",
              "name" -> Virtualizer.pushVirtual(t.getInternalName).apply(0)
            )
            frame.locals(target) = clsObj.address
          case x: scala.Byte  => B.write(x, w)
          case x: scala.Char  => C.write(x, w)
          case x: scala.Short => S.write(x, w)
          case x: scala.Int   => I.write(x, w)
          case x: scala.Float => F.write(x, w)
          case x: scala.Long  => J.write(x, w)
          case x: scala.Double => D.write(x, w)
        }
      case UnaryOp(src, dest, op) =>
        op.out.write(op.func(op.a.read(reader(frame.locals, src))), writer(frame.locals, dest))

      case BinOp(a, b, dest, op) =>
        op.out.write(
          op.func(
            op.b.read(reader(frame.locals, b)),
            op.a.read(reader(frame.locals, a))
          ), writer(frame.locals, dest)
        )

      case PutStatic(src, cls, index, prim) =>
        System.arraycopy(frame.locals, src, cls.statics, index, prim.size)

      case GetStatic(src, cls, index, prim) =>
        System.arraycopy(cls.statics, index, frame.locals, src, prim.size)

      case PutField(src, obj, index, prim) =>
        blit(frame.locals, src, frame.locals(obj).obj.members, index, prim.size)

      case GetField(src, obj, index, prim) =>
        blit(frame.locals(obj).obj.members, index, frame.locals, src, prim.size)

      case BinaryBranch(symA, symB, target, src, phi) =>
        val (a, b) = (frame.locals(symA), frame.locals(symB))
        if(src.pred(b, a)) {
          for ((src, dest) <- phi) frame.locals(dest) = frame.locals(src)
          frame.pc = target
        }

      case UnaryBranch(sym, target, src, phi) =>
        if(src.pred(frame.locals(sym))) {
          for ((src, dest) <- phi) frame.locals(dest) = frame.locals(src)

          frame.pc = target
        }

      case Goto(target, phi) =>
        for ((symA, symB) <- phi){
          frame.locals(symB) = frame.locals(symA)

        }
        frame.pc = target
      case CheckCast(src, desc) =>
        frame.locals(src) match{
          case 0 => ()
          case top if (top.isArr && !check(top.arr.tpe, desc)) || (top.isObj && !check(top.obj.tpe, desc)) =>
            throwExWithTrace("java/lang/ClassCastException", "")
          case _ => ()
        }
      case InstanceOf(src, dest, desc) =>
        frame.locals(dest) = frame.locals(src) match{
          case 0 => 1
          case top if (top.isArr && !check(top.arr.tpe, desc)) || (top.isObj && !check(top.obj.tpe, desc)) =>
            0
          case _ => 1
        }
      case MultiANewArray(desc, symbol, dims) =>
        def rec(dims: List[Int], tpe: imm.Type): Val = {

          (dims, tpe) match {
            case (size :: tail, imm.Type.Arr(innerType: imm.Type.Ref)) =>
              val newArr = vrt.Arr.allocate(innerType, size)
              for(i <- 0 until size){
                newArr(i) = rec(tail, innerType)
              }
              newArr.address

            case (size :: Nil, imm.Type.Arr(innerType)) =>
              vrt.Arr.allocate(innerType, size).address
          }
        }
        val dimValues = dims.map(frame.locals).toList

        val array = rec(dimValues, desc)
        frame.locals(symbol) = array
      case AThrow(src) =>
        this.throwException(frame.locals(src).obj)
    }
  }catch{case e: Throwable if !e.isInstanceOf[WrappedVmException] =>
    val newEx = new InternalVmException(e)
    newEx.setStackTrace(trace)
    throw newEx
  }

  def trace = {

    threadStack.map( f =>
      new StackTraceElement(
        f.runningClass.name.toDot,
        f.method.method.name,
        f.runningClass.clsData.misc.sourceFile.getOrElse("<unknown file>"),
        f.method.lineNumbers(f.pc-1)
      )
    ).toArray
  }

  def returnVal(size: Int, index: Int) = {
//    println("RETURNING " + size + " "  + index)
    for (i <- 0 until size){
      frame.returnTo(frame.locals(index + i))
    }
    this.threadStack.pop
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
                       args: Seq[Int],
                       returnTo: Int => Unit) = {
    println(indent + "PrepInvoke " + mRef + " with " + args)

    mRef match{
      case rt.Method.Native(clsName, imm.Sig(name, desc), op) =>
        op(this, reader(args, 0), returnTo)
      case m @ rt.Method.Cls(cls, methodIndex, method) =>

        assert((m.method.access & Access.Native) == 0, "method cannot be native: " + cls.name + " " + method.sig.unparse)
        val startFrame = new Frame(
          runningClass = cls,
          method = m,
          returnTo = returnTo,
          locals = args.toArray.padTo(m.localsSize, 0)
        )

        //log(indent + "locals " + startFrame.locals)
        threadStack.push(startFrame)
    }
  }
  final def prepInvoke(tpe: imm.Type,
                       sig: imm.Sig,
                       args: Seq[Any],
                       returnTo: Int => Unit)
                       : Unit = {

    val tmp = mutable.Buffer.empty[Val]

    args.map(
      Virtualizer.pushVirtual(_, tmp.append(_: Int))
    )

    prepInvoke(
      vm.resolveDirectRef(tpe.cast[imm.Type.Cls], sig).get,
      tmp,
      returnTo
    )

  }
  def invoke(mRef: rt.Method, args: Seq[Int]): Any = {
    val startHeight = threadStack.length
    prepInvoke(mRef, args, writer(returnedVal, 0))

    while(threadStack.length != startHeight) step()


    Virtualizer.popVirtual(mRef.sig.desc.ret, reader(returnedVal, 0))
  }

  def invoke(cls: imm.Type.Cls, sig: imm.Sig, args: Seq[Any]): Any = {
    val startHeight = threadStack.length
    prepInvoke(cls, sig, args, writer(returnedVal, 0))

    while(threadStack.length != startHeight) step()
    Virtualizer.popVirtual(sig.desc.ret, reader(returnedVal, 0))
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
            val returnTo: Int => Unit,
            val locals: Array[Val])


