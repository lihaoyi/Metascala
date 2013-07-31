package metascala
package rt

import scala.collection.mutable
import annotation.tailrec


import imm.Access
import metascala.opcodes.{TryCatchBlock, Invoke, Jump, Insn}
import Insn._

import metascala.imm.Attached.LineNumber
import Insn.Push
import Insn.InvokeStatic
import scala.Some
import metascala.UncaughtVmException
import Insn.ReturnVal
import imm.Type.Prim._
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


  def doPhi(frame: Frame, oldBlock: Int, newBlock: Int) = {
//    println(indent + "doPhi")
//    println(indent + oldBlock + "\t" + newBlock)

    val (srcs, dests) = frame.method.code.blocks(newBlock).phi(oldBlock).unzip
//    println(indent + (srcs, dests))
    val temp = srcs.map(frame.locals)
    java.util.Arrays.fill(frame.locals, 0)
    for ((i, dest) <- temp.zip(dests)){

      frame.locals(dest) = i
    }
    (srcs, dests)
  }

  def jumpPhis(target: Int) = {
    doPhi(frame, frame.pc._1, target)
    frame.pc = (target, 0)
  }
  final def step(): Unit = try {
    //  println(frame.pc)
    val code = frame.method.code

    var block = code.blocks(frame.pc._1)
    while(block.insns.length == 0){
      jumpPhis(frame.pc._1 + 1)
      block = code.blocks(frame.pc._1)
    }

    val node = block.insns(frame.pc._2)

    val r = reader(frame.locals, 0)

    if (frame.method.method.name == "stringSwitch") {
      lazy val localSnapshot =
        block.locals
             .flatMap(x => Seq(x.prettyRead(r)).padTo(x.size, "~"))
             .toList

//      println(indent + "::\t" + frame.runningClass.shortName + "/" + frame.method.sig.shortName + ": " + localSnapshot)
//      println(indent + "::\t" + frame.pc + "\t" + node )
    }
//
//    println(indent + "::\t" + vm.heap.dump().replace("\n", "\n" + indent + "::\t"))
    val currentFrame = frame

    def advancePc() = {
      if (currentFrame.pc._2 + 1 < code.blocks(currentFrame.pc._1).insns.length){
        currentFrame.pc =(currentFrame.pc._1, currentFrame.pc._2 + 1)
        Nil
      }else if(currentFrame.pc._1 + 1 < code.blocks.length){
        val phis = doPhi(currentFrame, currentFrame.pc._1, currentFrame.pc._1+1)
        currentFrame.pc = (currentFrame.pc._1+1, 0)
        phis._1.zip(phis._2)
      }else {
        Nil
      }
    }

    node match {
      case ReturnVal(sym) =>
        returnVal(frame.method.method.sig.desc.ret.size, sym)

      case Push(target, prim, value) =>
        prim.write(value, writer(frame.locals, target))
        advancePc()

      case New(target, cls) =>
        cls.checkInitialized()
        val obj = vrt.Obj.allocate(cls.name)
        frame.locals(target) = obj.address
        advancePc()

      case InvokeStatic(target, sources, owner, m) =>
        owner.checkInitialized()
        // Check for InvokeSpecial, which gets folded into InvokeStatic
        val thisVal = sources.length > m.sig.desc.args.length
        val thisCell = if (thisVal) Seq(1) else Nil

        val args =
            sources.zip(thisCell ++ m.sig.desc.args.map(_.size))
                   .flatMap{case (s, t) => frame.locals.slice(s, s + t)}

        if (thisVal && args(0) == 0){
          throwExWithTrace("java/lang/NullPointerException", "null")
        }else{
          val phis = advancePc()
          val ptarget = phis.toMap.getOrElse(target, target)
          prepInvoke(m, args, writer(frame.locals, ptarget))
        }

      case InvokeVirtual(target, sources, owner, sig, mIndex) =>
        val args = sources.zip(1 +: sig.desc.args.map(_.size))
                          .flatMap{case (s, t) =>
          frame.locals.slice(s, s + t)
        }
        val phis = advancePc()
        val ptarget = phis.toMap.getOrElse(target, target)
        if(args(0) == 0) throwExWithTrace("java/lang/NullPointerException", "null")
        else {
          val mRef = mIndex match{
            case -1 =>
              args(0).obj.cls.vTableMap(sig)
            case _ =>
              val cls =
                if (args(0).isObj) args(0).obj.cls
                else owner.cls
              cls.vTable(mIndex)
          }
          prepInvoke(mRef, args, writer(frame.locals, ptarget))
        }

      case ArrayLength(src, dest) =>
        frame.locals(dest) = frame.locals(src).arr.arrayLength
        advancePc()
      case NewArray(src, dest, typeRef) =>
        val newArray = vrt.Arr.allocate(typeRef, frame.locals(src))
        frame.locals(dest) = newArray.address
        advancePc()
      case PutArray(src, index, array, prim) =>
        val arr = frame.locals(array).arr
        if (0 <= frame.locals(index) && frame.locals(index) < arr.arrayLength){
          blit(frame.locals, src, arr, frame.locals(index) * prim.size, prim.size)
          advancePc()
        }else{
          throwExWithTrace("java/lang/ArrayIndexOutOfBoundsException", frame.locals(index).toString)
        }

      case GetArray(dest, index, array, prim) =>
        val arr = frame.locals(array).arr
        if (0 <= frame.locals(index) && frame.locals(index) < arr.arrayLength){
          blit(arr, frame.locals(index) * prim.size, frame.locals, dest, prim.size)
          advancePc()
        }else{
          throwExWithTrace("java/lang/ArrayIndexOutOfBoundsException", frame.locals(index).toString)
        }
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
        advancePc()
      case UnaryOp(src, psrc, dest, pout, func) =>
        pout.write(func(psrc.read(reader(frame.locals, src))), writer(frame.locals, dest))
        advancePc()
      case BinOp(a, pa, b, pb, dest, pout, func) =>

        val va = pa.read(reader(frame.locals, a))
        val vb = pb.read(reader(frame.locals, b))

        val out = func(va, vb)

        pout.write(out, writer(frame.locals, dest))
        advancePc()
      case PutStatic(src, cls, index, prim) =>
        cls.checkInitialized()
        System.arraycopy(frame.locals, src, cls.statics, index, prim.size)
        advancePc()
      case GetStatic(src, cls, index, prim) =>
        cls.checkInitialized()
        System.arraycopy(cls.statics, index, frame.locals, src, prim.size)

        advancePc()
      case PutField(src, obj, index, prim) =>
        blit(frame.locals, src, frame.locals(obj).obj.members, index, prim.size)
        advancePc()
      case GetField(src, obj, index, prim) =>
        blit(frame.locals(obj).obj.members, index, frame.locals, src, prim.size)
        advancePc()
      case BinaryBranch(symA, symB, target, func) =>
        val (a, b) = (frame.locals(symA), frame.locals(symB))
        if(func(b, a)) jumpPhis(target)
        else advancePc()


      case UnaryBranch(sym, target, func) =>
        if(func(frame.locals(sym))) jumpPhis(target)
        else advancePc()


      case Goto(target) =>
        jumpPhis(target)

      case TableSwitch(src, min, max, default, targets) =>
        var done = false
        for ((k, t) <- min to max zip targets) yield {
          if (frame.locals(src) == k && !done){
            jumpPhis(t)
            done = true
          }
        }
        if (!done) jumpPhis(default)
      case LookupSwitch(src, default, keys, targets) =>
        var done = false
        for ((k, t) <- keys zip targets) yield {
          if (frame.locals(src) == k && !done){
            jumpPhis(t)
            done = true
          }
        }
        if (!done) jumpPhis(default)

      case CheckCast(src, dest, desc) =>
        frame.locals(src) match{

          case top if (top.isArr && !check(top.arr.tpe, desc)) || (top.isObj && !check(top.obj.tpe, desc)) =>
            throwExWithTrace("java/lang/ClassCastException", "")
          case _ =>
            frame.locals(dest) = frame.locals(src)
            advancePc()
        }
      case InstanceOf(src, dest, desc) =>
        frame.locals(dest) = frame.locals(src) match{
          case 0 => 1
          case top if (top.isArr && !check(top.arr.tpe, desc)) || (top.isObj && !check(top.obj.tpe, desc)) =>
            0
          case _ => 1
        }
        advancePc()
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
        advancePc()
      case AThrow(src) =>
        this.throwException(frame.locals(src).obj)
    }


    opCount += 1

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
        0
      )
    ).toArray
  }

  def returnVal(size: Int, index: Int) = {
//    println(s"Returning size: $size, index: $index")
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
    import math.Ordering.Implicits._

    threadStack.headOption match{
      case Some(frame)=>
        val handler =
          frame.method.code.tryCatches.find{x =>
            (x.start <= frame.pc) &&
              (x.end >= frame.pc) &&
              !x.blockType.isDefined ||
              x.blockType.map(ex.cls.typeAncestry.contains).getOrElse(false)
          }

        handler match{
          case None =>
            threadStack.pop()
            throwException(ex, false)
          case Some(TryCatchBlock(start, end, handler, dest, blockType)) =>
            frame.locals(dest) = ex.address
            frame.pc = (handler, 0)

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
//    println(indent + "PrepInvoke " + mRef + " with " + args)

    mRef match{
      case rt.Method.Native(clsName, imm.Sig(name, desc), op) =>
        op(this, reader(args, 0), returnTo)
      case m @ rt.Method.Cls(cls, methodIndex, method) =>

        assert((m.method.access & Access.Native) == 0, "method cannot be native: " + cls.name + " " + method.sig.unparse)
        val startFrame = new Frame(
          runningClass = cls,
          method = m,
          returnTo = returnTo,
          locals = args.toArray.padTo(m.code.localSize, 0)
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
class Frame(var pc: (Int, Int) = (0, 0),
            val runningClass: rt.Cls,
            val method: rt.Method.Cls,
            var lineNum: Int = 0,
            val returnTo: Int => Unit,
            val locals: Array[Val])
