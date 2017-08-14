package metascala
package rt

import scala.collection.mutable
import annotation.tailrec
import metascala.opcodes.{BasicBlock, Insn, TryCatchBlock}
import Insn._
import Insn.Push
import Insn.InvokeStatic
import Insn.ReturnVal
import metascala.imm.{Desc, Sig, Type}
import metascala.natives.Bindings
import metascala.util._

import scala.reflect.ClassTag

object Thread{
  trait VMInterface extends opcodes.SingleInsnSSAConverter.VMInterface{
    def insnLimit: Long
    def checkInitialized(cls: rt.Cls): Unit
    def threads: Seq[Thread]
    def offHeap: Array[Byte]
    def setOffHeapPointer(n: Long): Unit
    def offHeapPointer: Long
    def currentThread: Int
    def internedStrings: mutable.Map[String, Ref]
    def natives: Bindings
    def check(s: imm.Type, t: imm.Type): Boolean
    def logger: Logger
  }

  /**
    * The stack frame created by every method call
    */
  class Frame(var pc: (Int, Int) = (0, 0),
              val runningClass: rt.Cls,
              val method: rt.ClsMethod,
              var lineNum: Int = 0,
              val returnTo: Int => Unit,
              val locals: Array[Int])

  trait Logger{
    def active: Boolean
    def logStep(indentCount: Int,
                clsName: String,
                frame: Frame,
                node: Insn,
                block: BasicBlock): Unit
    def logPhi(indentCount: Int, clsName: String, shifts: Iterator[(Int, Int)]): Unit
  }
  object NonLogger extends Logger{
    def active = false
    def logStep(indentCount: Int,
                clsName: String,
                frame: Frame,
                node: Insn,
                block: BasicBlock): Unit = ()
    def logPhi(indentCount: Int, clsName: String, shifts: Iterator[(Int, Int)]): Unit = ()
  }
}
/**
 * A single thread within the Metascala VM.
 */
class Thread(val threadStack: mutable.ArrayStack[Thread.Frame] = mutable.ArrayStack())
            (implicit val vm: Thread.VMInterface) { thread =>
  import vm._

  private[this] var opCount = 0L
  def getOpCount = opCount
  def frame = threadStack.top

  val returnedVal = Array(0, 0)
  private[this] var insnCount = 0L
  def count = insnCount
  def indent = threadStack.length



  def doPhi(frame: Thread.Frame, oldBlock: Int, newBlock: Int) = {
    lazy val output = mutable.Buffer.empty[fansi.Str]

    val phi = frame.method.code.blocks(newBlock).phi(oldBlock)
    val temp = phi.map(x => frame.locals(x._1))
    if (vm.logger.active) vm.logger.logPhi(
      indent,
      ClsTable.clsIndex(frame.method.clsIndex).name,
      (0 until temp.length).iterator.map(i => (i, phi(i)._2))
    )
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

    if (vm.logger.active) vm.logger.logStep(
      indent,
      ClsTable.clsIndex(frame.method.clsIndex).name,
      frame,
      node,
      block
    )

    val currentFrame = frame

    def advancePc() = {
      if (currentFrame.pc._2 + 1 < code.blocks(currentFrame.pc._1).insns.length){
        currentFrame.pc =(currentFrame.pc._1, currentFrame.pc._2 + 1)
        Agg.empty
      }else if(currentFrame.pc._1 + 1 < code.blocks.length){
        val phi = doPhi(currentFrame, currentFrame.pc._1, currentFrame.pc._1+1)
        currentFrame.pc = (currentFrame.pc._1+1, 0)
        phi
      }else {
        Agg.empty
      }
    }

    node match {
      case ReturnVal(sym) =>
        returnVal(frame.method.sig.desc.ret.size, sym)

      case Push(target, prim, value) =>
        prim.write(value, Util.writer(frame.locals, target))
        advancePc()

      case New(target, clsIndex) =>
        val cls = ClsTable.clsIndex(clsIndex)
        checkInitialized(cls)
        val obj = vm.alloc(_.newObj(cls.name))
        frame.locals(target) = obj.address()
        advancePc()

      case InvokeStatic(target, sources, clsIndex, mIndex, special) =>
        val cls = ClsTable.clsIndex(clsIndex)
        checkInitialized(cls)
        // Check for InvokeSpecial, which gets folded into InvokeStatic
        val m =
          if (special) cls.vTable(mIndex)
          else cls.staticTable(mIndex)

        val thisCell = sources.length > m.sig.desc.args.length

        if (thisCell && frame.locals(sources(0)) == 0){
          throwExWithTrace("java/lang/NullPointerException", "null")
        }else{
          val args = new Aggregator[Int]

          if (thisCell) args.append(frame.locals(sources(0)))


          for(i <- 0 until m.sig.desc.args.length){
            if (thisCell){
              args.append(frame.locals(sources(i + 1)))
              if (m.sig.desc.args(i).size == 2) args.append(frame.locals(sources(i + 1) + 1))
            } else /* i != 0 && !thisCell.isDefined */{
              args.append(frame.locals(sources(i)))
              if (m.sig.desc.args(i).size == 2) args.append(frame.locals(sources(i) + 1))
            }
          }

          val phis = advancePc()
          val ptarget = phis.find(_._1 == target).fold(target)(_._2)
          prepInvoke(m, args, Util.writer(frame.locals, ptarget))
        }

      case InvokeVirtual(target, sources, clsIndex, sig, mIndex) =>

        val argZero = frame.locals(sources(0))

        if(argZero == 0) throwExWithTrace("java/lang/NullPointerException", "null")
        else {
          val mRef = mIndex match{
            case -1 =>
              vm.obj(argZero).cls.vTableMap(sig)
            case _ =>
              val cls =
                if (vm.isObj(argZero)) vm.obj(argZero).cls
                else ClsTable.clsIndex(clsIndex)
              cls.vTable(mIndex)
          }

//          println("prepInvoke! " + ptargets)
          val args = new Aggregator[Int]
          for(i <- sources.indices){
            if (i == 0) args.append(frame.locals(sources(i)))
            else for(j <- 0 until sig.desc.args(i-1).size){
              args.append(frame.locals(sources(i) + j))
            }
          }

          val phis = advancePc()
          val ptargets = phis.collect{case (`target`, x) => x}
          val mapped = ptargets.map(Util.writer(frame.locals, _))
          prepInvoke(
            mRef,
            args,
            if (phis.isEmpty) Util.writer(frame.locals, target)
            else (x: Int) => mapped.foreach(_(x))
          )
        }
      case InvokeDynamic(name, desc, bsTag, bsOwner, bsName, bsDesc, bsArgs) =>
        invoke(bsOwner, imm.Sig(bsName, Desc.read(bsDesc)), Agg.from(bsArgs))
        ???
      case ArrayLength(src, dest) =>
        frame.locals(dest) = vm.arr(frame.locals(src)).arrayLength
        advancePc()

      case NewArray(src, dest, typeRef) =>
        val newArray = vm.alloc(_.newArr(typeRef, frame.locals(src)))
        frame.locals(dest) = newArray.address()
        advancePc()

      case PutArray(src, index, array, prim) =>
        val arr = vm.arr(frame.locals(array))
        if (0 <= frame.locals(index) && frame.locals(index) < arr.arrayLength){
          Util.blit(frame.locals, src, arr, frame.locals(index) * prim.size, prim.size)
          advancePc()
        }else{
          throwExWithTrace("java/lang/ArrayIndexOutOfBoundsException", frame.locals(index).toString)
        }

      case GetArray(dest, index, array, prim) =>
        val arr = vm.arr(frame.locals(array))
        if (0 <= frame.locals(index) && frame.locals(index) < arr.arrayLength){
          Util.blit(arr, frame.locals(index) * prim.size, frame.locals, dest, prim.size)
          advancePc()
        }else{
          throwExWithTrace("java/lang/ArrayIndexOutOfBoundsException", frame.locals(index).toString)
        }

      case Ldc(target, index) =>
        frame.locals(target) = vm.interned(index)()
        advancePc()

      case UnaryOp(src, psrc, dest, pout, func) =>
        pout.write(func(psrc.read(Util.reader(frame.locals, src))), Util.writer(frame.locals, dest))
        advancePc()

      case BinOp(a, pa, b, pb, dest, pout, func) =>

        val va = pa.read(Util.reader(frame.locals, a))
        val vb = pb.read(Util.reader(frame.locals, b))
        val out = func(va, vb)

        pout.write(out, Util.writer(frame.locals, dest))
        advancePc()
      case PutStatic(src, clsIndex, index, prim) =>
        val cls = ClsTable.clsIndex(clsIndex)
        checkInitialized(cls)
        Util.blit(frame.locals, src, new rt.Arr(cls.statics)(vm), index, prim.size)
        advancePc()
      case GetStatic(src, clsIndex, index, prim) =>
        val cls = ClsTable.clsIndex(clsIndex)
        checkInitialized(cls)
        Util.blit(new rt.Arr(cls.statics)(vm), index, frame.locals, src, prim.size)
        advancePc()

      case PutField(src, obj, index, prim) =>
        if (frame.locals(obj) == 0){
          throwExWithTrace("java/lang/NullPointerException", "null")
        }else{
          Util.blit(frame.locals, src, vm.obj(frame.locals(obj)).members, index, prim.size)
          advancePc()
        }

      case GetField(src, obj, index, prim) =>
        if (frame.locals(obj) == 0){
          throwExWithTrace("java/lang/NullPointerException", "null")
        }else{
          Util.blit(vm.obj(frame.locals(obj)).members, index, frame.locals, src, prim.size)
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
        val keys = min to max
        for(i <- keys.indices){
          val k = keys(i)
          val t = targets(i)
          if (frame.locals(src) == k && !done){
            jumpPhis(t)
            done = true
          }
        }
        if (!done) jumpPhis(default)
      case LookupSwitch(src, default, keys, targets) =>
        var done = false
        for(i <- keys.indices){
          val k = keys(i)
          val t = targets(i)
          if (frame.locals(src) == k && !done){
            jumpPhis(t)
            done = true
          }
        }
        if (!done) jumpPhis(default)

      case CheckCast(src, dest, desc) =>
        frame.locals(src) match{
          case top
            if (vm.isArr(top) && !check(vm.arr(top).tpe, desc))
            || (vm.isObj(top) && !check(vm.obj(top).tpe, desc)) =>

            throwExWithTrace("java/lang/ClassCastException", "")
          case _ =>
            frame.locals(dest) = frame.locals(src)
            advancePc()
        }
      case InstanceOf(src, dest, desc) =>
        frame.locals(dest) = frame.locals(src) match{
          case 0 => 0
          case top if (vm.isArr(top) && !check(vm.arr(top).tpe, desc)) || (vm.isObj(top) && !check(vm.obj(top).tpe, desc)) =>
            0
          case _ => 1
        }
        advancePc()

      case MultiANewArray(desc, symbol, dims) =>
        def rec(dims: List[Int], tpe: imm.Type): Int = {

          (dims, tpe) match {
            case (size :: tail, imm.Type.Arr(innerType: imm.Type.Ref)) =>
              val newArr = vm.alloc(_.newArr(innerType, size))
              for(i <- 0 until size){
                newArr(i) = rec(tail, innerType)
              }
              newArr.address()

            case (size :: Nil, imm.Type.Arr(innerType)) =>
              vm.alloc(_.newArr(innerType, size)).address()
           }
        }
        val dimValues = dims.map(frame.locals).toList

        val array = rec(dimValues, desc)
        frame.locals(symbol) = array
        advancePc()
      case AThrow(src) =>
        this.throwException(vm.obj(frame.locals(src)))
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
        f.runningClass.name.replace('/', '.'),
        f.method.sig.name,
        f.runningClass.sourceFile.getOrElse("<unknown file>"),
        try f.method.code.blocks(f.pc._1).lines(f.pc._2)
        catch{case _:Throwable => 0 }
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
        r.newObj(clsName,
          "stackTrace" -> r.register(Virtualizer.toVirtObj(trace)),
          "detailMessage" -> r.register(Virtualizer.toVirtObj(detailMessage))
        )
      )
    )
  }

  @tailrec final def throwException(ex: Obj, print: Boolean = true): Unit = {
    import math.Ordering.Implicits._
    if (print)println("Throwing!")

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
          Virtualizer.toRealObj[Throwable](ex.address())(bindingsInterface, implicitly)
        )
    }
  }
  val bindingsInterface = new Bindings.Interface{
    def throwExWithTrace(clsName: String, detailMessage: String) = {
      thread.throwExWithTrace(clsName, detailMessage)
    }

    def invoke(cls: Type.Cls, sig: Sig, args: Agg[Any]) = thread.invoke0(cls, sig, args)

    def invoke(mRef: Method, args: Agg[Int]) = thread.invoke0(mRef, args)

    def returnedVal = thread.returnedVal

    def alloc[T](func: rt.Allocator => T) = vm.alloc(func)

    def invokeRun(a: Int) = {
      val pa = obj(a)
      val mRef = resolveDirectRef(pa.cls.tpe, pa.cls.methods.find(_.sig.name == "run").get.sig).get
      var x = 0
      threads(0).invoke0(mRef, Agg(pa.address()))

      threads(0).returnedVal(0)
    }
    def newInstance(constr: Int, argArr: Int): Int = alloc{r =>
      val cls = r.obj(constr).apply("clazz")
      val name = toRealObj[String](r.obj(cls).apply("name")).replace('.', '/')
      val newObj = alloc { implicit r =>
        r.newObj(name).address()
      }

      val descStr = toRealObj[String](r.obj(constr).apply("signature"))

      val mRef = ClsTable(name).method(
        "<init>",
        Desc.read(descStr)
      ).get

      invoke(
        mRef,
        Agg(newObj) ++ (if (argArr == 0) Seq() else Agg.from(arr(argArr)))
      )

      newObj
    }

    def typeObjCache = vm.typeObjCache

    def threads = vm.threads

    def offHeap = vm.offHeap
    def setOffHeapPointer(n: Long) = vm.setOffHeapPointer(n)
    def offHeapPointer = vm.offHeapPointer

    def runningClassName(n: Int) = threadStack(n).runningClass.name

    def threadStackLength = threadStack.length

    def internedStrings = vm.internedStrings

    def theUnsafe = vm.theUnsafe

    def toRealObj[T](x: Int)(implicit ct: ClassTag[T]) = Virtualizer.toRealObj(x)(this, ct)

    def toVirtObj(x: Any)(implicit registrar: rt.Allocator) = vm.obj(Virtualizer.toVirtObj(x))

    def trace = thread.trace

    def currentThread = vm.currentThread

    implicit def ClsTable = vm.ClsTable

    def heap = vm.heap

    def obj(address: Int) = vm.obj(address)

    def arr(address: Int) = vm.arr(address)

    def isArr(address: Int) = vm.isArr(address)

    def isObj(address: Int) = vm.isObj(address)

    def natives = vm.natives

    def lookupNatives(lookupName: String, lookupSig: imm.Sig) = vm.natives.trapped.find{case rt.NativeMethod(clsName, sig, func) =>
      (lookupName == clsName) && sig == lookupSig
    }
  }
  final def prepInvoke(mRef: rt.Method,
                       args: Agg[Int],
                       returnTo: Int => Unit) = {
//    println(indent + "PrepInvoke " + mRef + " with " + args)

    mRef match{
      case rt.NativeMethod(clsName, imm.Sig(name, desc), op) =>
          op(bindingsInterface, Util.reader(args.toArray, 0), returnTo)
      case m @ rt.ClsMethod(clsIndex, methodIndex, sig, static, codethunk) =>

        assert(!m.native, "method cannot be native: " + ClsTable.clsIndex(clsIndex).name + " " + sig.unparse)
        val padded = args.toArray.padTo(m.code.localSize, 0)
        val startFrame = new Thread.Frame(
          runningClass = ClsTable.clsIndex(clsIndex),
          method = m,
          returnTo = returnTo,
          locals = padded
        )

        //log(indent + "locals " + startFrame.locals)
        threadStack.push(startFrame)
    }
  }
  final def prepInvoke(tpe: imm.Type,
                       sig: imm.Sig,
                       args: Agg[Any],
                       returnTo: Int => Unit)
                       : Unit = {

    val tmp = new Aggregator[Int]
    vm.alloc{ implicit r =>
      for(x <- args){
        Virtualizer.pushVirtual(x, tmp.append(_: Int))
      }


      prepInvoke(
        vm.resolveDirectRef(tpe.asInstanceOf[imm.Type.Cls], sig).get,
        tmp,
        returnTo
      )
    }

  }
  def invoke(mRef: rt.Method, args: Agg[Int]): Any = {
    invoke0(mRef, args)
    Virtualizer.popVirtual(mRef.sig.desc.ret, Util.reader(returnedVal, 0))(bindingsInterface)
  }
  def invoke0(mRef: rt.Method, args: Agg[Int]): Any = {
    val startHeight = threadStack.length
    prepInvoke(mRef, args, Util.writer(returnedVal, 0))

    while(threadStack.length != startHeight) step()



  }

  def invoke(cls: imm.Type.Cls, sig: imm.Sig, args: Agg[Any]): Any = {
    invoke0(cls, sig, args)
    Virtualizer.popVirtual(sig.desc.ret, Util.reader(returnedVal, 0))(bindingsInterface)
  }
  def invoke0(cls: imm.Type.Cls, sig: imm.Sig, args: Agg[Any]): Any = {
    val startHeight = threadStack.length
    prepInvoke(cls, sig, args, Util.writer(returnedVal, 0))

    while(threadStack.length != startHeight) step()

  }
}

