package metascala
package rt

import scala.collection.mutable
import annotation.tailrec


import imm.Access
import metascala.opcodes.{TryCatchBlock, Invoke, Jump, Insn}
import Insn._


import Insn.Push
import Insn.InvokeStatic
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
  private[this] var insnCount = 0L
  def count = insnCount
  def indent = "\t" * threadStack.filter(_.method.sig.name != "Dummy").length


  def doPhi(frame: Frame, oldBlock: Int, newBlock: Int) = {
    val phi = frame.method.code.blocks(newBlock).phi(oldBlock)
    val temp = phi.map(x => frame.locals(x._1))
    java.util.Arrays.fill(frame.locals, 0)
    for (i <- 0 until temp.length){
      val (src, dest) = (temp(i), phi(i)._2)
      frame.locals(dest) = src
    }
    phi
  }

  def jumpPhis(target: Int) = {
    doPhi(frame, frame.pc._1, target)
    frame.pc = (target, 0)
  }
  final def step(): Unit = try {
    //  println(frame.pc)
    val code = frame.method.code
    insnCount += 1
    var block = code.blocks(frame.pc._1)
    while(block.insns.length == 0){
      jumpPhis(frame.pc._1 + 1)
      block = code.blocks(frame.pc._1)
    }

    val node = block.insns(frame.pc._2)

    val r = reader(frame.locals, 0)

//    if (!threadStack.exists(x => x.method.sig.name == "<clinit>" && x.runningClass.name.contains("java/"))) {
//      lazy val localSnapshot =
//        block.locals
//             .flatMap(x => Seq(x.prettyRead(r)).padTo(x.size, "~"))
//             .toList
//
//      println(indent + "::\t" + frame.runningClass.shortName + "/" + frame.method.sig.shortName + ":" + block.lines(frame.pc._2) + "\t"  + localSnapshot)
//      println(indent + "::\t" + frame.pc + "\t" + node )
//      println(indent + "::\t" + vm.heap.dump().replace("\n", "\n" + indent + "::\t"))
//    }


    val currentFrame = frame

    def advancePc() = {
      if (currentFrame.pc._2 + 1 < code.blocks(currentFrame.pc._1).insns.length){
        currentFrame.pc =(currentFrame.pc._1, currentFrame.pc._2 + 1)
        Nil
      }else if(currentFrame.pc._1 + 1 < code.blocks.length){
        val phi = doPhi(currentFrame, currentFrame.pc._1, currentFrame.pc._1+1)
        currentFrame.pc = (currentFrame.pc._1+1, 0)
        phi
      }else {
        Nil
      }
    }

    node match {
      case ReturnVal(sym) =>
        returnVal(frame.method.sig.desc.ret.size, sym)

      case Push(target, prim, value) =>
        prim.write(value, writer(frame.locals, target))
        advancePc()

      case New(target, cls) =>
        cls.checkInitialized()
        val obj = vm.alloc(rt.Obj.allocate(cls.name)(_))
        frame.locals(target) = obj.address()
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
          val ptarget = phis.find(_._1 == target).fold(target)(_._2)
          prepInvoke(m, args, writer(frame.locals, ptarget))
        }

      case InvokeVirtual(target, sources, owner, sig, mIndex) =>
        val args = sources.zip(1 +: sig.desc.args.map(_.size))
                          .flatMap{case (s, t) =>
          frame.locals.slice(s, s + t)
        }
        val phis = advancePc()


        val isNull = args(0) == 0

        if(isNull) {
          throwExWithTrace("java/lang/NullPointerException", "null")
        } else {
          val mRef = mIndex match{
            case -1 =>
              args(0).obj.cls.vTableMap(sig)
            case _ =>
              val cls =
                if (args(0).isObj) args(0).obj.cls
                else owner.cls
              cls.vTable(mIndex)
          }
          val ptarget = phis.find(_._1 == target).fold(target)(_._2)
          prepInvoke(mRef, args, writer(frame.locals, ptarget))
        }

      case ArrayLength(src, dest) =>
        frame.locals(dest) = frame.locals(src).arr.arrayLength
        advancePc()

      case NewArray(src, dest, typeRef) =>
        val newArray = vm.alloc(rt.Arr.allocate(typeRef, frame.locals(src))(_))
        frame.locals(dest) = newArray.address()
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

      case Ldc(target, index) =>
        frame.locals(target) = vm.interned(index())()
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
        blit(frame.locals, src, cls.statics, index, prim.size)
        advancePc()
      case GetStatic(src, cls, index, prim) =>
        cls.checkInitialized()
        blit(cls.statics, index, frame.locals, src, prim.size)
        advancePc()

      case PutField(src, obj, index, prim) =>
        if (frame.locals(obj) == 0){
          throwExWithTrace("java/lang/NullPointerException", "null")
        }else{
          blit(frame.locals, src, frame.locals(obj).obj.members, index, prim.size)
          advancePc()
        }

      case GetField(src, obj, index, prim) =>
        if (frame.locals(obj) == 0){
          throwExWithTrace("java/lang/NullPointerException", "null")
        }else{
          blit(frame.locals(obj).obj.members, index, frame.locals, src, prim.size)
          advancePc()
        }

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
          case top
            if (top.isArr && !check(top.arr.tpe, desc))
            || (top.isObj && !check(top.obj.tpe, desc)) =>

            throwExWithTrace("java/lang/ClassCastException", "")
          case _ =>
            frame.locals(dest) = frame.locals(src)
            advancePc()
        }
      case InstanceOf(src, dest, desc) =>
        frame.locals(dest) = frame.locals(src) match{
          case 0 => 0
          case top if (top.isArr && !check(top.arr.tpe, desc)) || (top.isObj && !check(top.obj.tpe, desc)) =>
            0
          case _ => 1
        }
        advancePc()

      case MultiANewArray(desc, symbol, dims) =>
        def rec(dims: List[Int], tpe: imm.Type): Val = {

          (dims, tpe) match {
            case (size :: tail, imm.Type.Arr(innerType: imm.Type.Ref)) =>
              val newArr = vm.alloc(rt.Arr.allocate(innerType, size)(_))
              for(i <- 0 until size){
                newArr(i) = rec(tail, innerType)
              }
              newArr.address()

            case (size :: Nil, imm.Type.Arr(innerType)) =>
              vm.alloc(rt.Arr.allocate(innerType, size)(_)).address()
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
    if (opCount > vm.insnLimit){
      throw new Exception("Ran out of instructions! Limit: " + vm.insnLimit)
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
        f.method.sig.name,
        f.runningClass.sourceFile.getOrElse("<unknown file>"),
        try f.method.code.blocks(f.pc._1).lines(f.pc._2) catch{case _ => 0 }
      )
    ).toArray
  }

  def returnVal(size: Int, index: Int) = {
//    println(s"Returning size: $size, index: $index")
    for(i <- 0 until size){
      frame.returnTo(frame.locals(index + i))
    }
    this.threadStack.pop
  }
  final def throwExWithTrace(clsName: String, detailMessage: String) = {

    throwException(
      vm.alloc( implicit r =>
        rt.Obj.allocate(clsName,
          "stackTrace" -> trace.toVirtObj,
          "detailMessage" -> detailMessage.toVirtObj
        )
      )
    )
  }

  @tailrec final def throwException(ex: Obj, print: Boolean = true): Unit = {
    import math.Ordering.Implicits._
    println("Throwing!")
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
            frame.locals(dest) = ex.address()
            frame.pc = (handler, 0)

        }
      case None =>
        throw new UncaughtVmException(
          ex.address().toRealObj[Throwable]
        )
    }
  }

  final def prepInvoke(mRef: rt.Method,
                       args: Seq[Int],
                       returnTo: Int => Unit) = {
//    println(indent + "PrepInvoke " + mRef + " with " + args)

    mRef match{
      case rt.Method.Native(clsName, imm.Sig(name, desc), op) =>
        try op(this, reader(args, 0), returnTo)
        catch{case e: Exception =>
          throwExWithTrace(e.getClass.getName.toSlash, e.getMessage)
        }
      case m @ rt.Method.Cls(clsIndex, methodIndex, sig, static, codethunk) =>

        assert(!m.native, "method cannot be native: " + ClsTable.clsIndex(clsIndex).name + " " + sig.unparse)
        val startFrame = new Frame(
          runningClass = ClsTable.clsIndex(clsIndex),
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
    vm.alloc{ implicit r =>
      args.map(
        Virtualizer.pushVirtual(_, tmp.append(_: Int))
      )

      prepInvoke(
        vm.resolveDirectRef(tpe.cast[imm.Type.Cls], sig).get,
        tmp,
        returnTo
      )
    }

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
