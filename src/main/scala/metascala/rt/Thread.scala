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
    opcode.op(this)
  }

  final def step() = {
    val insnsList = frame.method.insns
    val node = insnsList(frame.pc)

    println(indent + frame.runningClass.name + "/" + frame.method.sig.unparse + ": " + frame.stack)
    println(indent + "---------------------- " + frame.pc + "\t" + node )
    frame.pc += 1
    opCount += 1

    node.op(this)
  }
  def returnVal(x: Int) = {

    val oldTop = threadStack.pop()
    threadStack.headOption match{
      case Some(frame) => for (i <- 0 until x) frame.stack.push(oldTop.stack.pop())
      case None => returnedVal = popVirtual(oldTop.method.method.desc.ret, oldTop.stack.pop)
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
            frame.stack.push(ex.address)
        }
      case None =>
        throw new UncaughtVmException(ex.cls.clsData.tpe.unparse,
          "Uncaught VM Error!",
          Nil,
          Nil)
    }
  }

  def popVirtual(tpe: imm.Type, src: => Val): Any = {
    tpe match {
      case imm.Type.Prim('V') => ()
      case imm.Type.Prim('Z') => Z(src)
      case imm.Type.Prim('B') => B(src)
      case imm.Type.Prim('C') => C(src)
      case imm.Type.Prim('S') => S(src)
      case imm.Type.Prim('I') => I(src)
      case imm.Type.Prim('F') => F(src)
      case imm.Type.Prim('J') => J.pop(src)
      case imm.Type.Prim('D') => D.pop(src)
      case t @ imm.Type.Cls(name) =>
        val address = src
        println("popVirtual Obj " + address)
        val obj = vrt.unsafe.allocateInstance(Class.forName(name.replace('/', '.')))

        println(vm.Heap.dump)
        var index = 0
        for(field <- t.cls.clsData.fields.filter(!_.static)){
          val f = obj.getClass.getDeclaredField(field.name)
          f.setAccessible(true)
          f.set(obj, popVirtual(field.desc, {
            val x = vm.Heap(address + 2 + index)
            index += 1
            x
          }))
        }
        obj

      case t @ imm.Type.Arr(tpe) =>
        val address = src
        println("popVirtual Arr " + address)


        println(vm.Heap.dump)
        val clsObj = forName(tpe.unparse)
        val newArr = java.lang.reflect.Array.newInstance(clsObj, address.arr.length)
        println(newArr)
        for(i <- 0 until address.arr.length){
          val raw = vm.Heap(address + 2 + i)
          val cooked = tpe.unparse match{
            case "Z" => raw == 0
            case "B" => raw.toByte
            case "C" => raw.toChar
            case "S" => raw.toShort
            case "I" => raw.toInt
            case "F" => F.apply(raw)
            case "J" => J.apply(0, raw)
            case "D" => D.apply(0, raw)
            case x => ???
          }
          java.lang.reflect.Array.set(newArr, i, cooked)
        }
        newArr
    }
  }
  def forNameBoxed(name: String) = {
    name match{
      case "Z" => classOf[java.lang.Boolean]
      case "B" => classOf[java.lang.Byte]
      case "C" => classOf[java.lang.Character]
      case "S" => classOf[java.lang.Short]
      case "I" => classOf[java.lang.Integer]
      case "F" => classOf[java.lang.Float]
      case "J" => classOf[java.lang.Long]
      case "D" => classOf[java.lang.Double]
      case x => Class.forName(x)
    }
  }
  def forName(name: String) = {
    name match{
      case "Z" => classOf[Boolean]
      case "B" => classOf[Byte]
      case "C" => classOf[Char]
      case "S" => classOf[Short]
      case "I" => classOf[Int]
      case "F" => classOf[Float]
      case "J" => classOf[Long]
      case "D" => classOf[Double]
      case x => Class.forName(x)
    }
  }
  def pushVirtual(thing: Any, out: Val => Unit): Unit = {
    thing match {
      case b: Boolean => Z.push(b, out)
      case b: Byte    => B.push(b, out)
      case b: Char    => C.push(b, out)
      case b: Short   => S.push(b, out)
      case b: Int     => I.push(b, out)
      case b: Float   => F.push(b, out)
      case b: Long    => J.push(b, out)
      case b: Double  => D.push(b, out)
      case b: Array[_] =>
        println("pushVirtual Arr ")
        println(vm.Heap.dump)
        val arr = vrt.Arr.allocate(imm.Type.Arr.read(b.getClass.getName).innerType,
          b.map{x => var tmp = 0; pushVirtual(x, tmp = _); tmp}
        )
        println(vm.Heap.dump)
        out(arr.address)
        println(vm.Heap.dump)
        println("address " + arr.address)
        println(vm.Heap.dump)
        println("EndPushArray")
      case b: Any =>
        println("pushVirtual Obj")
        println(vm.Heap.dump)
        val obj = vrt.Obj.allocate(b.getClass.getName.replace('.', '/'))
        println(vm.Heap.dump)
        var index = 0
        for(field <- obj.cls.clsData.fields.filter(!_.static)){
          println("FIELD " + field.name)
          val f = b.getClass.getDeclaredField(field.name)
          f.setAccessible(true)

          pushVirtual(f.get(b), {x =>
            vm.Heap(obj.address + 2 + index) = x
            println("index " + index)
            index += 1
            println("index " + index)
          })
        }
        println(vm.Heap.dump)
        println("address " +  obj.address)
        println(vm.Heap.dump)
        out(obj.address)
        println(vm.Heap.dump)
        println("EndPushObject")
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


