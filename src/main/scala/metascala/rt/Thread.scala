package metascala
package rt

import scala.collection.mutable
import annotation.tailrec


import imm.Access
import metascala.opcodes.{Invoke, Jump, Insn}
import Insn._
import Insn.Ldc
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
    for ((i, dest) <- temp.zip(dests)){
      frame.locals(dest) = i
    }
    (srcs, dests)
  }

  final def step(): Unit = try {
    //  println(frame.pc)
    val code = frame.method.code

    val block = code.blocks(frame.pc._1)
    if (block.insns.length == 0){
      doPhi(frame, frame.pc._1, frame.pc._1 + 1)
      frame.pc = frame.pc.copy(_1 = frame.pc._1 + 1)

      step()
      return
    }
    val node = block.insns(frame.pc._2)

    val r = reader(frame.locals, 0)

    lazy val localSnapshot =
      code.blocks(frame.pc._1)
          .locals
          .map(_.prim)
          .flatMap(x => Seq(x.read(r).toString).padTo(x.size, "~"))
          .toList
          .toString

//    println(indent + "::\t" + frame.runningClass.name + "/" + frame.method.sig.unparse + ": " + localSnapshot)
//    println(indent + "::\t" + frame.pc + "\t" + node )
//    val stackH = threadStack.length

//    println(indent + "::\t" + vm.Heap.dump.replace("\n", "\n" + indent + "::\t"))
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
          val phis = advancePc()
          val ptarget = phis.toMap.getOrElse(target, target)
          prepInvoke(m, args, writer(frame.locals, ptarget))
        }

      case InvokeSpecial(target, sources, owner, sig) =>
        resolveDirectRef(owner, sig).map{ m =>
          val args = sources.zip(1 +: sig.desc.args.map(_.size))
            .flatMap{case (s, t) =>
            frame.locals.slice(s, s + t)
          }
          val phis = advancePc()
          val ptarget = phis.toMap.getOrElse(target, target)
          if(args(0) == 0) throwExWithTrace("java/lang/NullPointerException", "null")
          else prepInvoke(m, args, writer(frame.locals, ptarget))
        }
      case InvokeVirtual(target, sources, owner, sig) =>
        val args = sources.zip(1 +: sig.desc.args.map(_.size))
          .flatMap{case (s, t) =>
          frame.locals.slice(s, s + t)
        }
        ///println("INVOKE VIRTUAL " + args)
        val phis = advancePc()
        val ptarget = phis.toMap.getOrElse(target, target)
        if(args(0) == 0) throwExWithTrace("java/lang/NullPointerException", "null")
        else {
          //println(args(0).obj.cls.clsData.tpe)
          val mRef = args(0).obj.cls.vTableMap(sig)
          prepInvoke(mRef, args, writer(frame.locals, ptarget))
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
      case UnaryOp(src, psrc, dest, pout, func) =>
        pout.write(func(psrc.read(reader(frame.locals, src))), writer(frame.locals, dest))

      case BinOp(a, pa, b, pb, dest, pout, func) =>
        pout.write(
          func(
            pb.read(reader(frame.locals, b)),
            pa.read(reader(frame.locals, a))
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

      case BinaryBranch(symA, symB, target, func) =>
        val (a, b) = (frame.locals(symA), frame.locals(symB))
        if(func(b, a)) {
          doPhi(frame, frame.pc._1, target)
          frame.pc = (target, 0)
        }else{
          advancePc()
        }

      case UnaryBranch(sym, target, func) =>
        if(func(frame.locals(sym))) {
          doPhi(frame, frame.pc._1, target)
          frame.pc = (target, 0)
        }else{
          advancePc()
        }

      case Goto(target) =>
        doPhi(frame, frame.pc._1, target)
        frame.pc = (target, 0)
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

    if (!node.isInstanceOf[Jump] && !node.isInstanceOf[Invoke]) advancePc()
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
//    println(indent + "RETURNING " + size + " "  + index)
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
            x.start <= frame.pc._1 &&
              x.end >= frame.pc._1 &&
              !x.blockType.isDefined ||
              x.blockType.map(ex.cls.typeAncestry.contains).getOrElse(false)
          }.headOption

        handler match{
          case None =>
            threadStack.pop()
            throwException(ex, false)
          case Some(imm.TryCatchBlock(start, end, handler, blockType)) =>
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


