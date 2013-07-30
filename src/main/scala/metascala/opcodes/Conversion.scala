package metascala
package opcodes

import metascala.imm.{Attached, Method}
import org.objectweb.asm.Type
import scala.collection.mutable
import imm.Type.Prim
import imm.Type.Prim._
import scala.annotation.tailrec
import org.objectweb.asm.tree._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.Opcodes._
import Insn._
import metascala.StackOps.{F1, F2}

class Box(val x: BasicValue) extends Value{
  override def getSize = x.getSize
  override def toString = hashCode.toString.take(2) + x.toString + " "
}
class FunnyInterpreter extends Interpreter[Box](ASM4){
  val internal = new BasicInterpreter()
  type AIN = AbstractInsnNode

  def newValue(tpe: org.objectweb.asm.Type) =
    if (tpe == null) new Box(BasicValue.UNINITIALIZED_VALUE)
    else if (tpe.getSort == Type.VOID) null
    else new Box(internal.newValue(tpe))

  def newOperation(insn: AIN) = new Box(internal.newOperation(insn))
  def copyOperation(insn: AIN, value: Box) = value
  def unaryOperation(insn: AIN, value: Box) = new Box(internal.unaryOperation(insn, value.x))
  def binaryOperation(insn: AIN, v1: Box, v2: Box) = new Box(internal.binaryOperation(insn, v1.x, v2.x))
  def ternaryOperation(insn: AIN, v1: Box, v2: Box, v3: Box) = new Box(internal.ternaryOperation(insn, v1.x, v2.x, v3.x))
  def naryOperation(insn: AIN, vs: java.util.List[_ <: Box]) = new Box(internal.naryOperation(insn, vs.asScala.map(_.x).asJava))
  def returnOperation(insn: AIN, value: Box, expected: Box) = internal.returnOperation(insn, value.x, expected.x)
  def merge(v1: Box, v2: Box) = v1
}
object Conversion {
  implicit def unbox(b: Box) = b.x
  def ssa(clsName: String, method: MethodNode)(implicit vm: VM): Code = {
    println(s"================Converting $clsName/${method.name}=========================")
    val insns = method.instructions.toArray
    println("Insns " + method.instructions.size)
    val aframes = new Analyzer(new FunnyInterpreter()).analyze(clsName, method)
    println("Analyzed")
    val frames = aframes.asInstanceOf[Array[Frame[Box]]]
    println("Frames " + frames.length)

    val jumps = insns.collect{case x: JumpInsnNode => x}
    val returns = Seq(RETURN, IRETURN, FRETURN, LRETURN, DRETURN, ARETURN, ATHROW)
    val exits = insns.filter(c => returns.contains(c.getOpcode))
    println("B")
    val lookupSwitches = insns.collect{case x: LookupSwitchInsnNode => x}
    val tableSwitches = insns.collect{case x: TableSwitchInsnNode => x}
    println("C")

    val bases =
      exits ++
      jumps ++
      lookupSwitches ++
      tableSwitches

    val targets =
      jumps.map(_.label) ++
      lookupSwitches.flatMap(_.labels.toArray) ++
      lookupSwitches.map(_.dflt) ++
      tableSwitches.flatMap(_.labels.toArray) ++
      tableSwitches.map(_.dflt) ++
      method.tryCatchBlocks.asScala.map(_.handler)

    println("Targets")
    targets.foreach(println)

    val blockMap = {
      val map = new Array[Int](insns.length)
      for(i <- 1 until insns.length){
        val prev = map(i - 1)
        map(i) = prev max map(i)
        if (bases.contains(insns(i)) && i + 1 < map.length){
          map(i + 1) = prev + 1
        }else if (targets.contains(insns(i))){
          map(i) = prev + 1
        }
      }
      map
    }

    val insnMap = {
      val map = new Array[Int](insns.length)
      var count = 0
      for(i <- 1 until insns.length){
        count += 1
        if (blockMap(i) != blockMap(i-1)) count = 0
        map(i) = count
      }
      map
    }

    implicit class pimpedFrame(x: Frame[Box]){
      def top(n: Int) = x.getStack(x.getStackSize - 1 - n)
      def boxes = {
        val locals = for {
          localId <- 0 until x.getLocals
          local <- Option(x.getLocal(localId))
          if local != BasicValue.UNINITIALIZED_VALUE
          if local.getType != null
        } yield local
        val stackVals = for {
          stackId <- 0 until x.getStackSize
          stackVal = x.getStack(stackId)
        } yield stackVal
        locals ++ stackVals
      }
    }
    println("=========================================")
    val blockBuffers = for(blockId <- 0 to blockMap.last) yield {
      val start = blockMap.indexOf(blockId)
      val end = blockMap.lastIndexOf(blockId)
      val buffer = mutable.Buffer.empty[Insn]
      val srcMap = mutable.Buffer.empty[Int]

      val localMap = mutable.Map.empty[Box, Int]
      var symCount = 0
      val types = mutable.Buffer.empty[imm.Type]


      implicit def getBox(b: Box) = {
        if (!localMap.contains(b)){
          localMap(b) = symCount
          symCount += b.x.getSize
          import org.objectweb.asm.Type
          types.append(b.x.getType.getSort match{
            case Type.BOOLEAN => Z
            case Type.CHAR    => C
            case Type.BYTE    => B
            case Type.SHORT   => S
            case Type.INT     => I
            case Type.FLOAT   => F
            case Type.LONG    => J
            case Type.DOUBLE  => D
            case Type.ARRAY   => imm.Type.Cls("java/lang/Object")
            case Type.OBJECT  => imm.Type.Cls("java/lang/Object")
            case _ => ???
          })
        }
        localMap(b)
      }

      // force in-order registration of method arguments in first block
      frames(0).boxes.map(getBox)
      println("BLOCK MAP " + blockMap.toSeq)
      for (i <- start to end){
        def append(in: Insn) = {
          buffer.append(in)
          srcMap.append(i - start)
        }

        implicit def deref(label: LabelNode) = blockMap(insns.indexOf(label))

        def push[T](v: T, tpe: Prim[T]) = {
          append(Push(frames(i+1).top(0), tpe, v))
        }
        def aload[T](p: Prim[T], tpe: imm.Type) ={
          append(GetArray(frames(i+1).top(0), frames(i).top(0), frames(i).top(1), tpe))
        }
        def astore[T](p: Prim[T], tpe: imm.Type) ={
          append(PutArray(frames(i).top(0), frames(i).top(1), frames(i).top(2), tpe))
        }
        def binop[A, B, C](a: Prim[A], b: Prim[B], c: Prim[C])(f: (A, B) => C) = {
          val (bb, aa) = (getBox(frames(i).top(0)), getBox(frames(i).top(1)))
          append(BinOp(aa, a, bb, b, frames(i+1).top(0), c, f))
        }
        def unaryop[A, B](a: Prim[A], b: Prim[B])(f: A => B) = {
          append(UnaryOp(frames(i).top(0), a, frames(i+1).top(0), b, f))
        }
        def unarybranch(label: LabelNode, f: Int => Boolean) = {
          append(Insn.UnaryBranch(frames(i).top(0), label, f))
        }
        def binarybranch(label: LabelNode, f: (Int, Int) => Boolean) = {
          append(Insn.BinaryBranch(frames(i).top(0), frames(i).top(1), label, f))
        }
        def returnval(tpe: imm.Type) = {
          append(Insn.ReturnVal(if (tpe == V) 0 else frames(i).top(0)))
        }
        println(blockId + "\t" + i + "\t" + insns(i))

        (insns(i).getOpcode, insns(i)) match{
          case (ICONST_0, _) => push(0, I)
          case (ICONST_1, _) => push(1, I)
          case (ICONST_2, _) => push(2, I)
          case (ICONST_3, _) => push(3, I)
          case (ICONST_4, _) => push(4, I)
          case (ICONST_5, _) => push(5, I)
          case (LCONST_0, _) => push(0L, J)
          case (LCONST_1, _) => push(1L, J)
          case (FCONST_0, _) => push(0F, F)
          case (FCONST_1, _) => push(1F, F)
          case (FCONST_2, _) => push(2F, F)
          case (DCONST_0, _) => push(0D, D)
          case (DCONST_1, _) => push(1D, D)
          case (BIPUSH, x: IntInsnNode) => push(x.operand, I)
          case (SIPUSH, x: IntInsnNode) => push(x.operand, I)
          case (LDC, x: LdcInsnNode) => append(Insn.Ldc(frames(i+1).top(0), x.cst))
          case (IALOAD, _) => aload(I, I)
          case (LALOAD, _) => aload(J, J)
          case (FALOAD, _) => aload(F, F)
          case (DALOAD, _) => aload(D, D)
          case (AALOAD, _) => aload(I, imm.Type.Cls("java/lang/Object"))
          case (BALOAD, _) => aload(B, B)
          case (CALOAD, _) => aload(C, C)
          case (SALOAD, _) => aload(S, S)
          case (IASTORE, _) => astore(I, I)
          case (LASTORE, _) => astore(J, J)
          case (FASTORE, _) => astore(F, F)
          case (DASTORE, _) => astore(D, D)
          case (AASTORE, _) => astore(I, imm.Type.Cls("java/lang/Object"))
          case (BASTORE, _) => astore(B, B)
          case (CASTORE, _) => astore(C, C)
          case (SASTORE, _) => astore(S, S)
          case (IADD, _) => binop(I, I, I)(F2(_ + _, "IAdd"))
          case (LADD, _) => binop(J, J, J)(F2(_ + _, "LAdd"))
          case (FADD, _) => binop(F, F, F)(F2(_ + _, "FAdd"))
          case (DADD, _) => binop(D, D, D)(F2(_ + _, "DAdd"))
          case (ISUB, _) => binop(I, I, I)(F2(_ - _, "ISub"))
          case (LSUB, _) => binop(J, J, J)(F2(_ - _, "LSub"))
          case (FSUB, _) => binop(F, F, F)(F2(_ - _, "FSub"))
          case (DSUB, _) => binop(D, D, D)(F2(_ - _, "DSub"))
          case (IMUL, _) => binop(I, I, I)(F2(_ * _, "IMul"))
          case (LMUL, _) => binop(J, J, J)(F2(_ * _, "LMul"))
          case (FMUL, _) => binop(F, F, F)(F2(_ * _, "FMul"))
          case (DMUL, _) => binop(D, D, D)(F2(_ * _, "DMul"))
          case (IDIV, _) => binop(I, I, I)(F2(_ / _, "IDiv"))
          case (LDIV, _) => binop(J, J, J)(F2(_ / _, "LDiv"))
          case (FDIV, _) => binop(F, F, F)(F2(_ / _, "FDiv"))
          case (DDIV, _) => binop(D, D, D)(F2(_ / _, "DDiv"))
          case (IREM, _) => binop(I, I, I)(F2(_ % _, "IRem"))
          case (LREM, _) => binop(J, J, J)(F2(_ % _, "LRem"))
          case (FREM, _) => binop(F, F, F)(F2(_ % _, "FRem"))
          case (DREM, _) => binop(D, D, D)(F2(_ % _, "DRem"))
          case (INEG, _) => unaryop(I, I)(F1(-_, "INeg"))
          case (LNEG, _) => unaryop(J, J)(F1(-_, "LNeg"))
          case (FNEG, _) => unaryop(F, F)(F1(-_, "FNeg"))
          case (DNEG, _) => unaryop(D, D)(F1(-_, "DNeg"))
          case (ISHL, _) => binop(I, I, I)(F2(_ << _, "IShl"))
          case (LSHL, _) => binop(J, I, J)(F2(_ << _, "LShl"))
          case (ISHR, _) => binop(I, I, I)(F2(_ >> _, "IShr"))
          case (LSHR, _) => binop(J, I, J)(F2(_ >> _, "LShr"))
          case (IUSHR, _) => binop(I, I, I)(F2(_ >>> _, "IUShr"))
          case (LUSHR, _) => binop(J, I, J)(F2(_ >>> _, "LUShr"))
          case (IAND, _) => binop(I, I, I)(F2(_ & _, "IAnd"))
          case (LAND, _) => binop(J, J, J)(F2(_ & _, "LAnd"))
          case (IOR, _) => binop(I, I, I)(F2(_ | _, "IOr"))
          case (LOR, _) => binop(J, J, J)(F2(_ | _, "LOr"))
          case (IXOR, _) => binop(I, I, I)(F2(_ ^ _, "IXOr"))
          case (LXOR, _) => binop(J, J, J)(F2(_ ^ _, "LXOr"))
          case (IINC, x: IincInsnNode) =>
            val bvalue = new Box(new BasicValue(org.objectweb.asm.Type.INT_TYPE))
            append(Insn.Push(bvalue, I, x.incr))
            append(Insn.BinOp[I, I, I](bvalue, I, frames(i).getLocal(x.`var`), I, frames(i+1).top(0), I, F2[Int, Int, Int](_+_, "IInc")))
          case (I2L, _) => unaryop(I, J)(F1(_.toLong,  "I2L"))
          case (I2F, _) => unaryop(I, F)(F1(_.toFloat, "I2F"))
          case (I2D, _) => unaryop(I, D)(F1(_.toDouble,"I2D"))
          case (L2I, _) => unaryop(J, I)(F1(_.toInt,   "L2I"))
          case (L2F, _) => unaryop(J, F)(F1(_.toFloat, "L2F"))
          case (L2D, _) => unaryop(J, D)(F1(_.toDouble,"L2D"))
          case (F2I, _) => unaryop(F, I)(F1(_.toInt,   "F2I"))
          case (F2L, _) => unaryop(F, J)(F1(_.toLong,  "F2L"))
          case (F2D, _) => unaryop(F, D)(F1(_.toDouble,"F2D"))
          case (D2I, _) => unaryop(D, I)(F1(_.toInt,   "D2I"))
          case (D2L, _) => unaryop(D, F)(F1(_.toLong,  "D2L"))
          case (D2F, _) => unaryop(D, F)(F1(_.toFloat, "D2F"))
          case (I2B, _) => unaryop(I, B)(F1(_.toByte,  "I2B"))
          case (I2C, _) => unaryop(I, C)(F1(_.toChar,  "I2C"))
          case (I2S, _) => unaryop(I, S)(F1(_.toShort, "I2S"))
          case (LCMP,  _) => binop(J, J, I)(F2(_ compare _, "LCmp"))
          case (FCMPL, _) => binop(F, F, I)(F2(_ compare _, "FCmpl"))
          case (FCMPG, _) => binop(F, F, I)(F2(_ compare _, "FCmpg"))
          case (DCMPL, _) => binop(D, D, I)(F2(_ compare _, "DCmpl"))
          case (DCMPG, _) => binop(D, D, I)(F2(_ compare _, "DCmpG"))
          case (IFEQ, x: JumpInsnNode) => unarybranch(x.label, F1(_ == 0, "IfEq"))
          case (IFNE, x: JumpInsnNode) => unarybranch(x.label, F1(_ != 0, "IfNe"))
          case (IFLT, x: JumpInsnNode) => unarybranch(x.label, F1(_ < 0,  "IfLt"))
          case (IFGE, x: JumpInsnNode) => unarybranch(x.label, F1(_ >= 0, "IfGe"))
          case (IFGT, x: JumpInsnNode) => unarybranch(x.label, F1(_ > 0,  "IfGt"))
          case (IFLE, x: JumpInsnNode) => unarybranch(x.label, F1(_ <= 0, "IfLe"))
          case (IF_ICMPEQ, x: JumpInsnNode) => binarybranch(x.label, F2(_ == _, "IfICmpEq"))
          case (IF_ICMPNE, x: JumpInsnNode) => binarybranch(x.label, F2(_ != _, "IfICmpNe"))
          case (IF_ICMPLT, x: JumpInsnNode) => binarybranch(x.label, F2(_ < _,  "IfICmpLt"))
          case (IF_ICMPGE, x: JumpInsnNode) => binarybranch(x.label, F2(_ >= _, "IfICmpGe"))
          case (IF_ICMPGT, x: JumpInsnNode) => binarybranch(x.label, F2(_ > _,  "IfICmpGt"))
          case (IF_ICMPLE, x: JumpInsnNode) => binarybranch(x.label, F2(_ <= _, "IfICmpLe"))
          case (IF_ACMPEQ, x: JumpInsnNode) => binarybranch(x.label, F2(_ == _, "IfACmpEq"))
          case (IF_ACMPNE, x: JumpInsnNode) => binarybranch(x.label, F2(_ != _, "IfACmpNe"))
          case (GOTO, x: JumpInsnNode) => append(Insn.Goto(x.label))
          case (TABLESWITCH, x: TableSwitchInsnNode)   =>
            append(Insn.TableSwitch(frames(i).top(0), x.min, x.max, x.dflt, x.labels.map(deref)))
          case (LOOKUPSWITCH, x: LookupSwitchInsnNode) =>
            append(Insn.LookupSwitch(frames(i).top(0), x.dflt, x.keys.asScala.map(_.intValue), x.labels.map(deref)))
          case (IRETURN, _) => returnval(I)
          case (LRETURN, _) => returnval(J)
          case (FRETURN, _) => returnval(F)
          case (DRETURN, _) => returnval(D)
          case (ARETURN, _) => returnval(imm.Type.Cls("java/lang/Object"))
          case (RETURN,  _) => returnval(V)
          case (GETSTATIC, x: FieldInsnNode) =>
            val index = x.owner.cls.staticList.indexWhere(_.name == x.name)
            val prim = x.owner.cls.staticList(index).desc
            append(Insn.GetStatic(frames(i+1).top(0), x.owner.cls, index, prim))
          case (PUTSTATIC, x: FieldInsnNode) =>
            val index = x.owner.cls.staticList.indexWhere(_.name == x.name)
            val prim = x.owner.cls.staticList(index).desc
            append(Insn.PutStatic(frames(i).top(0), x.owner.cls, index, prim))
          case (GETFIELD,  x: FieldInsnNode) =>
            val index = x.owner.cls.fieldList.lastIndexWhere(_.name == x.name)
            val prim = x.owner.cls.fieldList(index).desc
            append(Insn.GetField(frames(i+1).top(0), frames(i).top(0), index, prim))
          case (PUTFIELD,  x: FieldInsnNode) =>
            val index = x.owner.cls.fieldList.lastIndexWhere(_.name == x.name)
            val prim = x.owner.cls.fieldList(index).desc
            append(Insn.PutField(frames(i).top(1), frames(i).top(0), index, prim))
          case (INVOKESTATIC, x: MethodInsnNode) =>
            val desc = imm.Desc.read(x.desc)
            val m = vm.resolveDirectRef(x.owner, imm.Sig(x.name, desc)).get
            var j = 0
            val args = for(arg <- desc.args)yield{
              println(j)
              val res = frames(i).top(j)
              j += 1
              res
            }
            println(frames(i+1))
            val target =
              if (desc.ret == V)0
              else frames(i+1).top(0): Int

            append(Insn.InvokeStatic(target, args.map(getBox), x.owner, m))
          case (INVOKESPECIAL, x: MethodInsnNode) =>
          case (INVOKEVIRTUAL, x: MethodInsnNode) =>
          case (INVOKEINTERFACE, x: MethodInsnNode) =>
          case _ => ()
        }
      }


      val startFrame = frames(start)
      val origEndFrame = frames(end)
      val endFrame = if (origEndFrame != null){
        val f = new Frame[Box](origEndFrame)
        f.execute(insns(end), new FunnyInterpreter())
        f
      }else null

      (buffer, types, localMap, startFrame, endFrame)

    }

    for(i <- 0 until frames.length){
      println(insns(i).toString.drop(23).padTo(30, ' ') + (""+frames(i)).padTo(40, ' ') + " | " + blockMap(i) + " | " + insnMap(i))
    }

    println("++++++++++++++++++++++++++++++++++++++++")

    val basicBlocks = for(((buffer, types, localMap, startFrame, _), i) <- blockBuffers.zipWithIndex) yield {
      val phis = for(((buffer2, types2, localMap2, _, endFrame), j) <- blockBuffers.zipWithIndex) yield {
        if (endFrame != null && startFrame != null && ((buffer2.length > 0 && buffer2.last.targets.contains(i)) || (i == j + 1)))
          for{
            (e, s) <- endFrame.boxes zip startFrame.boxes

            a <- localMap2.get(e)
            b <- localMap.get(s)
          } yield (a, b)
        else Nil
      }
      BasicBlock(buffer, phis, types)
    }

    println("----------------------------------------------")
    for ((block, i) <- basicBlocks.zipWithIndex){
      println()
      println(i + "\t" + block.phi.toList)
      println(i + "\t" + block.locals)
      block.insns.foreach(println)
    }

    println("---TryCatch---")
//    tryCatchBlocks.foreach(println)
    println("----------------------------------------------")

    Code(basicBlocks, Nil)
  }
  /*
  def convertToSsa(cls: rt.Cls, method: Method)(implicit vm: VM): Code = {

    if (cls.name == "aaorg/objectweb/asm/ClassReader") {
      println(s"-------------------Converting: ${cls.name}/${method.sig}--------------------------")
      method.code.insns.zipWithIndex.foreach{ case (x, i) =>
        println(s"$i\t$x")
      }
      println("---TryCatch---")
      method.misc.tryCatchBlocks.foreach{ b =>
        println(b.start + " - " + b.end + ":\t" + b.handler)
      }
    }

    val (blocks, tryCatchBlocks) = walkBlocks(cls, method)

    val basicBlocks =
      blocks.map(b => b._2 -> b._4)
            .zip(makePhis(blocks))
            .map{ case ((buff, types), phis) => BasicBlock(buff, phis, types) }

    if(cls.name == "orgaa/objectweb/asm/ClassReader"){
      println("----------------------------------------------")
      for ((block, i) <- basicBlocks.zipWithIndex){
        println()
        println(i + "\t" + block.phi.toList)
        println(i + "\t" + block.locals)
        block.insns.foreach(println)
      }

      println("---TryCatch---")
      tryCatchBlocks.foreach(println)
      println("----------------------------------------------")
    }

    Code(basicBlocks, tryCatchBlocks)
  }

  /**
   * Calculates the phi node movements required at the top of each basic block
   * to complete the SSA code.
   */
  def makePhis(blocks: Blocks): Seq[Seq[Seq[(Sym, Sym)]]] = {
    val newBlocks = blocks
    for{((destState, buffer, _, _), i) <- newBlocks.zipWithIndex} yield {
      for{((_, srcBuffer, srcState, _), j) <- newBlocks.zipWithIndex} yield {

        def zipped = (srcState.locals zip destState.locals) ++
                     (srcState.stack.reverse zip destState.stack.reverse)
        def stuff = for {
          (src, dest) <- zipped.distinct
          if src.tpe.isRef == dest.tpe.isRef
          pairs <- src.slots zip dest.slots
        } yield pairs


        if (j + 1 == i) stuff
        else if(!srcBuffer.isEmpty && srcBuffer.last.targets.contains(i)) stuff
        else Nil
      }
    }
  }
  /**
   * Takes a method and breaks up the bytecode from a flat list of stack
   * operations into a list of basic blocks of register operations
   */
  def walkBlocks(cls: rt.Cls, method: imm.Method)(implicit vm: VM): (Blocks, Seq[opcodes.TryCatchBlock]) = {

    val locals: Vector[imm.Type] ={
      val thisArg =
        if(method.static) Nil
        else Seq(imm.Type.Cls("java/lang/Object"))

      method.desc
        .args
        .++:(thisArg)
        .toVector
    }

    var insns = method.code.insns.toList

    var attached = {
      val restAttached = method.code.attachments.toList
      (restAttached.head :+ imm.Attached.Frame(Nil, locals)) +: restAttached.tail
    }

    var state: State = null
    val allStates = mutable.Buffer.empty[State]

    val blockMap = new Array[Int](insns.length)
    val blocks = mutable.Buffer.empty[(State, Seq[Insn], State, Seq[Type])]
    var sections: Seq[Seq[Int]] =  Nil

    class SymbolMaker(){
      var symCount = 0
      val types = mutable.Buffer.empty[Type]
      def apply(t: Type) = {
        if (t != V){
          types.append(t)
        }
        val sym = Symbol(symCount, t)
        symCount += t.size
        sym
      }
    }

    while (insns != Nil){
      val makeSymbol = new SymbolMaker()
      val beginState =
        attached.head
                .collectFirst{case f: imm.Attached.Frame => f}
                .map{State(_, makeSymbol.apply _)}
                .getOrElse{
          new State(
            state.stack.map(s => makeSymbol(s.tpe)),
            state.locals.map(s => makeSymbol(s.tpe))
          )
        }

      allStates.append(beginState)

      val (regInsns, newInsns, newAttached, newState) = run(cls, insns, attached, allStates.last, makeSymbol.apply)
      sections = sections :+ regInsns.map(_.length).scan(0)(_+_)

      blockMap(method.code.insns.length - insns.length) = blocks.length
      blocks.append((allStates.last, regInsns.flatten, newState, makeSymbol.types))

      insns = newInsns
      attached = newAttached
      state = newState
    }

    var current = 0
    for (i <- 0 until blockMap.length){
      current = current max blockMap(i)
      blockMap(i) = current
    }

    val tryCatchBlocks = method.misc.tryCatchBlocks.map{b =>
      val (startBlock, endBlock) = (blockMap(b.start), blockMap(b.end))
      opcodes.TryCatchBlock(
        startBlock -> sections(startBlock)(b.start - blockMap.indexOf(startBlock)),
        endBlock -> sections(endBlock)(b.end - blockMap.indexOf(endBlock)),
        blockMap(b.handler),
        allStates(blockMap(b.handler)).stack(0).n,
        b.blockType
      )
    }
//    println("Block Map " + blockMap.toList)
    val finalBlocks = blocks.map{ case (before, buffer, after, types) =>
      val newBuffer = buffer.map{
        case x: Insn.UnaryBranch  => x.copy(target = blockMap(x.target))
        case x: Insn.BinaryBranch => x.copy(target = blockMap(x.target))
        case x: Insn.Goto         => x.copy(target = blockMap(x.target))
        case x: Insn.LookupSwitch => x.copy(targetList = x.targetList.map(blockMap), default = blockMap(x.default))
        case x: Insn.TableSwitch  => x.copy(targetList = x.targetList.map(blockMap), default = blockMap(x.default))
        case x => x
      }
      (before, newBuffer, after, types)
    }
    (finalBlocks, tryCatchBlocks)
  }
  @tailrec
  def run(cls: rt.Cls,
          insns: List[OpCode],
          attached: List[Seq[Attached]],
          state: State,
          makeSymbol: Type => Symbol,
          regInsns: Seq[Seq[Insn]] = Seq(),
          index: Int = 0)
         (implicit vm: VM): (Seq[Seq[Insn]], List[OpCode], List[Seq[Attached]], State) = {

    (insns, attached) match {
      case (i :: is, a :: as) =>

//        println(i + "\t" + i.toString.padTo(30, ' ') + state.stack.toString.padTo(20, ' ') + state.locals.toString.padTo(30, ' '))
        val (newState, newInsn) = op(cls, state, i, makeSymbol)
        val outInsns = regInsns ++ Seq(newInsn)
        import StackOps._
        (i, as) match{
          case (_, a :: _) if a.exists(_.isInstanceOf[Attached.Frame]) => (outInsns, is, as, newState)
          case (_: Jump | _: ReturnVal | AThrow, _) => (outInsns, is, as, newState)
          case _ => run(cls, is, as, newState, makeSymbol, outInsns, index + 1)
        }
      case _ => (regInsns, insns, attached, state)
    }
  }

  case class State(stack: List[Symbol], locals: Vector[Symbol]) extends imm.Attached
  object State{
    def apply(f: imm.Attached.Frame, makeSymbol: Type => Symbol): State = {
      State(
        f.stack.map(makeSymbol).flatMap(s => Seq.fill(s.size)(s)),
        f.locals.map(makeSymbol).flatMap(s => Seq.fill(s.size)(s))
      )
    }
  }

  import StackOps._

  implicit class poppable[T](val l: List[T]){
    def pop(p: Type) = {
      val t = l.splitAt(p.size)
      t.copy(_1 = t._1.head)
    }
  }

  def collapseReverse(in: List[Symbol], out: List[Symbol] = Nil): List[Symbol] = in match{
    case Nil => out
    case head :: rest => collapseReverse(rest.drop(head.size - 1), head :: out)
  }
  /**
   * Performs a single step in the symbolic interpretation of a method; does
   * everything pure-functional style, taking the old state in and spitting
   * the new state out the other end, togther with any register instructions
   * that are needed.
   */
  def op(cls: rt.Cls, state: State, oc: OpCode, makeSymbol: Type => Symbol)(implicit vm: VM): (State, Seq[Insn]) = oc match {

    case InvokeStatic(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize)
      val m = vm.resolveDirectRef(cls, sig).get
      val target = makeSymbol(sig.desc.ret)
      state.copy(stack = target join newStack) -> List(Insn.InvokeStatic(target.n, collapseReverse(args).map(_.n), cls, m))

    case InvokeSpecial(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val m = vm.resolveDirectRef(cls, sig).get
      val target = makeSymbol(sig.desc.ret)
      state.copy(stack = target join newStack) -> List(Insn.InvokeStatic(target.n, collapseReverse(args).map(_.n), cls, m))

    case InvokeVirtual(ref, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val target = makeSymbol(sig.desc.ret)
      val cls = ref match{
        case c: imm.Type.Cls => c
        case _ => imm.Type.Cls("java/lang/Object")
      }
      val mIndex = vm.ClsTable(cls).vTable.indexWhere(_.sig == sig)
      state.copy(stack = target join newStack) -> List(Insn.InvokeVirtual(target.n, collapseReverse(args).map(_.n), cls.cast[imm.Type.Cls], sig, mIndex))

    case InvokeInterface(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val target = makeSymbol(sig.desc.ret)
      state.copy(stack = target join newStack) -> List(Insn.InvokeVirtual(target.n, collapseReverse(args).map(_.n), cls.cast[imm.Type.Cls], sig, -1))

    case NewArray(typeCode) =>
      val length :: rest = state.stack

      val typeRef: imm.Type = typeCode match{
        case 4  => Z: imm.Type
        case 5  => C: imm.Type
        case 6  => F: imm.Type
        case 7  => D: imm.Type
        case 8  => B: imm.Type
        case 9  => S: imm.Type
        case 10 => I: imm.Type
        case 11 => J: imm.Type
      }
      val symbol = makeSymbol(imm.Type.Arr(typeRef))
      state.copy(stack = symbol :: rest) -> List(Insn.NewArray(length.n, symbol.n, typeRef))

    case MonitorEnter | MonitorExit =>
      val monitor :: rest = state.stack
      state.copy(stack = rest) -> Nil

    case ArrayLength =>
      val arr :: rest = state.stack
      val symbol = makeSymbol(I)
      state.copy(stack = symbol :: rest) -> List(Insn.ArrayLength(arr.n, symbol.n))

    case ANewArray(typeRef) =>
      val length :: rest = state.stack
      val symbol = makeSymbol(imm.Type.Arr(typeRef))
      state.copy(stack = symbol :: rest) -> List(Insn.NewArray(length.n, symbol.n, typeRef))

    case LoadArray(prim) =>
      val index :: array :: base = state.stack
      val symbol = makeSymbol(prim)

      state.copy(stack = symbol join base) -> List(Insn.GetArray(symbol.n, index.n, array.n, prim))

    case StoreArray(prim) =>
      val (value, index :: array :: base) = state.stack.pop(prim)
      state.copy(stack = base) -> List(Insn.PutArray(value.n, index.n, array.n, prim))

    case Load(index, _) =>
      state.copy(stack = state.locals(index) join state.stack) -> Nil

    case Ldc(thing) =>
      val symbol = thing match{
        case _: Long => makeSymbol(J)
        case _: Double => makeSymbol(D)
        case x: scala.Byte   => makeSymbol(B)
        case x: scala.Char   => makeSymbol(C)
        case x: scala.Short  => makeSymbol(S)
        case x: scala.Int    => makeSymbol(I)
        case x: scala.Float  => makeSymbol(F)
        case x: scala.Long   => makeSymbol(J)
        case x: scala.Double => makeSymbol(D)
        case _: String => makeSymbol(imm.Type.Cls("java/lang/String"))
        case _: org.objectweb.asm.Type => makeSymbol(imm.Type.Cls("java/lang/Class"))
      }

      state.copy(stack = symbol join state.stack) -> List(Insn.Ldc(symbol.n, thing))

    case BinOp(a, b, out, func) =>
      val symbol = makeSymbol(out)
      val (symA, stack1) = state.stack.pop(a)
      val (symB, stack2) = stack1.pop(b)

      state.copy(stack = symbol join stack2) -> List(Insn.BinOp(symA.n, a, symB.n, b, symbol.n, out, func))

    case UnaryOp(a, prim, func) =>
      val symbol = makeSymbol(prim)
      val (symA, stack1) = state.stack.pop(a)
      state.copy(stack = symbol join stack1) -> List(Insn.UnaryOp(symA.n, a, symbol.n, prim, func))

    case Store(index, p) =>
      val (popped, rest) = state.stack.pop(p)
      state.copy(stack = rest, locals = state.locals.padTo(index, Symbol(-1, V)).patch(index, Seq.fill(p.size)(popped), p.size)) -> Nil

    case UnaryBranch(index, func) =>
      val head :: tail = state.stack
      state.copy(stack = tail) -> List(Insn.UnaryBranch(head.n, index, func))

    case BinaryBranch(index, func) =>
      val first :: second :: newStack = state.stack
      state.copy(stack = newStack) -> List(Insn.BinaryBranch(first.n, second.n, index, func))

    case ReturnVal(n) =>
      val returned = if (n == 0) new Symbol(0, V) else state.stack.head
      state.copy(stack = state.stack.drop(n)) -> List(Insn.ReturnVal(returned.n))

    case Goto(index) =>
      state -> List(Insn.Goto(index))

    case Push(v) =>
      val symbol = makeSymbol(I)
      state.copy(stack = symbol join state.stack) -> List(Insn.Push(symbol.n, I, v))

    case Const(prim, value) =>
      val symbol = makeSymbol(prim)
      state.copy(stack = symbol join state.stack) -> List(Insn.Push(symbol.n, prim, value))

    case New(desc) =>
      val symbol = makeSymbol(desc)
      state.copy(stack = symbol join state.stack) ->
        List(Insn.New(symbol.n, vm.ClsTable(desc)))

    case PutStatic(owner, name, tpe) =>
      val index = owner.cls.staticList.indexWhere(_.name == name)
      val prim = owner.cls.staticList(index).desc
      val (symbol, rest) = state.stack.pop(prim)
      state.copy(stack = rest) ->
      List(Insn.PutStatic(symbol.n, owner.cls, index, prim))

    case GetStatic(owner, name, tpe) =>
      val index = owner.cls.staticList.indexWhere(_.name == name)
      val prim = owner.cls.staticList(index).desc
      val symbol = makeSymbol(prim)
      state.copy(stack = symbol join state.stack) ->
        List(Insn.GetStatic(symbol.n, owner.cls, index, prim))

    case PutField(owner, name, tpe) =>
      val index = owner.cls.fieldList.lastIndexWhere(_.name == name)
      val prim = owner.cls.fieldList(index).desc

      val (symA, stack1) = state.stack.pop(tpe)
      val (symB, stack2) = stack1.pop(I)

      state.copy(stack = stack2) ->
        List(Insn.PutField(symA.n, symB.n, index, prim))

    case GetField(owner, name, tpe) =>
      val index = owner.cls.fieldList.lastIndexWhere(_.name == name)
      val prim = owner.cls.fieldList(index).desc
      val symbol = makeSymbol(prim)
      val (sym, stack1) = state.stack.pop(tpe)

      state.copy(stack = symbol join stack1) ->
        List(Insn.GetField(symbol.n, sym.n, index, prim))

    case IInc(varId, amount) =>

      val symbol = makeSymbol(I)
      val out = makeSymbol(I)
      state.copy(locals = state.locals.updated(varId, out)) -> Seq(
        Insn.Push(symbol.n, I, amount),
        Insn.BinOp[I, I, I](symbol.n, I, state.locals(varId).n, I, out.n, I, F2[Int, Int, Int](_+_, "IInc"))
      )

    case ManipStack(transform) =>
      state.copy(stack = transform(state.stack).cast[List[Symbol]]) -> Nil

    case AThrow =>
      val ex :: rest = state.stack
      state.copy(stack = rest) -> Seq(Insn.AThrow(ex.n))

    case CheckCast(desc) =>
      state -> Seq(Insn.CheckCast(state.stack.head.n, desc))

    case LookupSwitch(default: Int, keys: Seq[Int], targets: Seq[Int]) =>
      state.copy(stack = state.stack.tail) -> Seq(
        Insn.LookupSwitch(state.stack.head.n, default, keys, targets)
      )
    case TableSwitch(min, max, default, targets) =>
      state.copy(stack = state.stack.tail) -> Seq(
        Insn.TableSwitch(state.stack.head.n, min, max, default, targets)
      )

    case InstanceOf(desc) =>
      val head :: rest = state.stack
      val symbol = makeSymbol(desc)
      state.copy(stack = symbol :: rest) -> Seq(Insn.InstanceOf(head.n, symbol.n, desc))

    case MultiANewArray(desc, dims) =>
      val (dimsX, rest) = state.stack.splitAt(dims)
      val symbol = makeSymbol(desc)
      state.copy(stack = symbol :: rest) -> Seq(Insn.MultiANewArray(desc, symbol.n, dimsX.map(_.n)))
  }*/
}