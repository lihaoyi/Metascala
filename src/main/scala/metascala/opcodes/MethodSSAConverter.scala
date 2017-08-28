package metascala
package opcodes

import org.objectweb.asm.Type

import scala.collection.mutable
import imm.Type.Prim._
import metascala.util.{Agg, Aggregator}
import org.objectweb.asm.tree._

import scala.collection.JavaConverters._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.Opcodes._

import scala.reflect.ClassTag

class AbstractFunnyInterpreter(mergeLeft: Boolean) extends Interpreter[Box](ASM4){
  val internal = new BasicInterpreter()
  type AIN = AbstractInsnNode

  def newValue(tpe: org.objectweb.asm.Type) =
    if (tpe == null) new Box(BasicValue.UNINITIALIZED_VALUE)
    else if (tpe.getSort == Type.VOID) null
    else new Box(internal.newValue(tpe))


  def newOperation(insn: AIN) = new Box(internal.newOperation(insn))
  def copyOperation(insn: AIN, value: Box) = value
  def unaryOperation(insn: AIN, value: Box) = new Box(internal.unaryOperation(insn, value.value))
  def binaryOperation(insn: AIN, v1: Box, v2: Box) = new Box(internal.binaryOperation(insn, v1.value, v2.value))
  def ternaryOperation(insn: AIN, v1: Box, v2: Box, v3: Box) = new Box(internal.ternaryOperation(insn, v1.value, v2.value, v3.value))
  def naryOperation(insn: AIN, vs: java.util.List[_ <: Box]) = new Box(internal.naryOperation(insn, vs.asScala.map(_.value).asJava))
  def returnOperation(insn: AIN, value: Box, expected: Box) = ()
  def merge(v1: Box, v2: Box) = if (mergeLeft) v1 else v2
}

object IncrementalInterpreter extends AbstractFunnyInterpreter(mergeLeft = false)
object FullInterpreter extends AbstractFunnyInterpreter(mergeLeft = true)

object MethodSSAConverter {
  val unconditionalJumps = Seq(GOTO, RETURN, ARETURN, IRETURN, FRETURN, LRETURN, DRETURN, ATHROW)

  def boxes(x: Frame[Box]) = {
    val locals = (0 until x.getLocals).map{ localId =>
      val local = x.getLocal(localId)
      if (local != null && local.value.getType != null) Some(local)
      else None
    }

    val stackVals =
      for (stackId <- 0 until x.getStackSize)
      yield Some(x.getStack(stackId))

    locals ++ stackVals
  }

  def computeFlatten[T: ClassTag](blockMap: Array[Int], allTs: IndexedSeq[T]) = {
    val arr = Array.fill(blockMap.last + 1)(mutable.Buffer.empty[T])
    for(i <- blockMap.indices){
      arr(blockMap(i)).append(allTs(i))
    }
    arr.map(_.toArray)
  }

  def apply(clsName: String, method: MethodNode)
           (implicit vm: SingleInsnSSAConverter.VMInterface): Code = {

    val allInsns = method.instructions.toArray
    assert(
      method.instructions.size != 0,
      "Unknown native method: " + clsName + " " + method.name + " " + method.desc
    )
    val insnIndexMap = allInsns.zipWithIndex.toMap

    val lineMap = {
      var lastLine = 0
      val lines = new Array[Int](allInsns.length)
      for((insn, i) <- allInsns.zipWithIndex){
        insn match{
          case x: LineNumberNode => lastLine = x.line
          case _ => ()
        }
        lines(i) = lastLine
      }
      lines
    }

    lazy val extraFrames = new Analyzer(FullInterpreter).analyze(clsName, method)

    val blockMap: Array[Int] = computeBlockMap(method, allInsns)

    val blockInsns: Array[Array[AbstractInsnNode]] = computeFlatten(blockMap, allInsns)

    val blockLines: Array[Array[Int]] = computeFlatten(blockMap, lineMap)

    val allFrames: Seq[Seq[Frame[Box]]] =
      computeAllFrames(method, extraFrames, blockMap, blockInsns, insnIndexMap)

    val blockBuffers = computeBlockBuffers(clsName, blockMap, blockInsns, blockLines, allFrames)

    val realInsn = blockBuffers.map(_._2)


    val basicBlocks = computeBasicBlocks(method, blockBuffers)

    val tryCatchBlocks = for(b <- Agg.from(method.tryCatchBlocks.asScala)) yield{
      def insnToPc(insn: AbstractInsnNode) = {
        val block = blockMap(insnIndexMap(insn))
        (block, realInsn(block)(blockInsns(block).indexOf(insn)))
      }

      val handlerBlock = blockMap(insnIndexMap(b.handler))

      TryCatchBlock(
        insnToPc(b.start), insnToPc(b.end),
        handlerBlock,
        blockBuffers(handlerBlock)._4 {
          val x = allFrames(blockMap(insnIndexMap(b.handler))).head
          x.getStack(x.getStackSize - 1)
        },
        Option(b.`type`).map(imm.Type.Cls.apply)
      )
    }

    vm.logger.logBasicBlocks(clsName, method, basicBlocks, blockBuffers.map(_._4), tryCatchBlocks)

    Code(basicBlocks, tryCatchBlocks)
  }

  def computeBlockMap(method: MethodNode, allInsns: Array[AbstractInsnNode]) = {

    val jumps = allInsns.collect { case x: JumpInsnNode => x }
    val returns = Seq(RETURN, IRETURN, FRETURN, LRETURN, DRETURN, ARETURN, ATHROW)
    val exits = allInsns.filter(c => returns.contains(c.getOpcode))
    val lookupSwitches = allInsns.collect { case x: LookupSwitchInsnNode => x }
    val tableSwitches = allInsns.collect { case x: TableSwitchInsnNode => x }
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

    val map = new Array[I](allInsns.length)
    for (i <- 1 until allInsns.length) {
      val prev = map(i - 1)
      map(i) = prev max map(i)
      if (bases.contains(allInsns(i)) && i + 1 < map.length) {
        map(i + 1) = prev + 1
      } else if (targets.contains(allInsns(i))) {
        map(i) = prev + 1
      }
    }
    map

  }

  def computeBlockBuffers(clsName: String,
                          blockMap: Array[I],
                          blockInsns: Array[Array[AbstractInsnNode]],
                          blockLines: Array[Array[I]],
                          allFrames: Seq[Seq[Frame[Box]]])
                         (implicit vm: SingleInsnSSAConverter.VMInterface)= {

    for {
      (blockFrames, blockId) <- Agg.from(allFrames).zipWithIndex
      if blockFrames != null
    } yield {

      val buffer = new Aggregator[Insn]

      val lineMap = new Aggregator[I]
      val localMap = mutable.Map.empty[Box, I]
      var symCount = 0
      val types = new Aggregator[LocalType]


      def getBox(b: Box): Int = {
        if (!localMap.contains(b)) {
          localMap(b) = symCount
          symCount += b.value.getSize
          assert(b != null)
          assert(b.value != null)
          assert(b.value.getType != null, "fail " + b.value)

          types.append(b.value match {
            case BasicValue.INT_VALUE => LocalType.Int
            case BasicValue.FLOAT_VALUE => LocalType.Float
            case BasicValue.LONG_VALUE => LocalType.Long
            case BasicValue.DOUBLE_VALUE => LocalType.Double
            case BasicValue.REFERENCE_VALUE => LocalType.Ref
          })
        }
        localMap(b)
      }

      boxes(blockFrames(0)).flatten.map(getBox)
      val realInsnMap = new Aggregator[Int]
      for ((insn, i) <- blockInsns(blockId).zipWithIndex) try {
        realInsnMap.append(buffer.length)

        SingleInsnSSAConverter(
          insn,
          (in: Insn) => {
            buffer.append(in)
            lineMap.append(blockLines(blockId)(i))
          },
          blockFrames(i),
          blockFrames(i + 1),
          label => blockInsns.indexWhere(_.head == label),
          getBox
        )
      } catch {
        case e: Throwable =>
          println("ConvertInsn Failed " + clsName)
          throw e
      }

      (buffer, realInsnMap, types, localMap, blockFrames.head, blockFrames.last, lineMap)
    }
  }

  def computeAllFrames(method: MethodNode,
                       extraFrames: => Array[Frame[Box]],
                       blockMap: Array[I],
                       blockInsns: Array[Array[AbstractInsnNode]],
                       insnIndexMap: AbstractInsnNode => Int) = {

    val links: Seq[(I, I)] = {
      val jumps =
        blockInsns.map(_.last)
          .zipWithIndex
          .flatMap {
            case (x: JumpInsnNode, i) => Seq(i -> blockMap(insnIndexMap(x.label)))
            case (x: TableSwitchInsnNode, i) => (x.dflt +: x.labels.asScala).map(x => i -> blockMap(insnIndexMap(x)))
            case (x: LookupSwitchInsnNode, i) => (x.dflt +: x.labels.asScala).map(x => i -> blockMap(insnIndexMap(x)))
            case _ => Nil
          }
      val flows =
        for {
          (a, b) <- (0 until blockMap.last).zip(1 to blockMap.last)
          if !unconditionalJumps.contains(blockInsns(a).last.getOpcode)
        } yield (a, b)
      jumps ++ flows
    }

    val existing = new Array[Seq[Frame[Box]]](blockInsns.length)

    def copy(f: Frame[Box]) = {
      val newF = new Frame[Box](f)
      newF.clearStack()
      for (i <- 0 until f.getLocals) newF.setLocal(i, new Box(f.getLocal(i).value))
      for (i <- 0 until f.getStackSize) newF.push(new Box(f.getStack(i).value))
      newF
    }

    def handle(startFrame: Frame[Box], blockId: I): Unit = {
      if (existing(blockId) != null) ()
      else {
        val theseFrames = blockInsns(blockId).scanLeft(startFrame) { (frame, insn) =>
          val f = new Frame(frame)
          if (insn.getOpcode != -1) {
            f.execute(insn, IncrementalInterpreter)
          }
          f
        }
        existing(blockId) = theseFrames
        for {
          (src, dest) <- links
          if src == blockId
        } handle(copy(theseFrames.last), dest)
      }
    }

    handle(extraFrames(0), 0)
    for (b <- method.tryCatchBlocks.asScala) {
      val catchFrame = extraFrames(insnIndexMap(b.handler))
      handle(catchFrame, blockInsns.indexWhere(_.head == b.handler))
    }
    existing

  }

  def computeBasicBlocks(method: MethodNode,
                         blockBuffers:  Agg[(Agg[Insn], Agg[Int], Agg[LocalType], mutable.Map[Box, Int], Frame[Box], Frame[Box], Agg[Int])]) = {
    for(((buffer, realInsnMap, types, startMap, startFrame, _, lines), i) <- blockBuffers.zipWithIndex) yield {
      val phis = for(((buffer2, realInsnMap2, types2, endMap, _, endFrame, _), j) <- blockBuffers.zipWithIndex) yield {
        if (endFrame != null && startFrame != null && ((buffer2.length > 0 && buffer2.last.targets.contains(i)) || (i == j + 1))){
          val phiLength =
            if (startMap.isEmpty) 0
            else startMap.map(x => x._2 + x._1.getSize).max

          val arr = Array.fill[Int](phiLength)(-1)

          for{
            (Some(e), Some(s)) <- boxes(endFrame) zip boxes(startFrame)
            src <- endMap.get(e).toSeq
            dest <- startMap.get(s).toSeq

            i <- 0 until e.getSize
          } arr(dest + i) = src + i
          //          println("zipped             " + zipped)
          Agg.from(arr)
        }else Agg.empty

      }
      BasicBlock(buffer, phis, types, lines)
    }
  }
}