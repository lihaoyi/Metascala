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

  def apply(clsName: String, method: MethodNode)
           (implicit vm: SingleInsnSSAConverter.VMInterface): Code = {

    val allInsns = method.instructions.toArray

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

    val blockMap: Array[I] = computeBlockMap(method, allInsns)

    val insnMap: Array[Int] = {
      val map = new Array[Int](allInsns.length)
      var count = 0
      for(i <- 1 until allInsns.length){
        count += 1
        if (blockMap(i) != blockMap(i-1)) count = 0
        map(i) = count
      }
      map
    }

    implicit class pimpedLabel(x: LabelNode){
      def block = blockMap(allInsns.indexOf(x))
      def insn = insnMap(allInsns.indexOf(x))
    }

    val blockInsns: Array[Array[AbstractInsnNode]] = {
      blockMap.zip(allInsns)
              .groupBy(_._1)
              .toArray
              .sortBy(_._1)
              .map(_._2.map(_._2))

    }

    val blockLines: Array[Array[Int]] = {
      blockMap.zip(lineMap)
              .groupBy(_._1)
              .toArray
              .sortBy(_._1)
              .map(_._2.map(_._2))
    }

    implicit def deref(label: LabelNode) = blockInsns.indexWhere(_.head == label)
//    println("blockInsns")

    val allFrames: Seq[Seq[Frame[Box]]] =
      computeAllFrames(method, allInsns, extraFrames, blockMap, blockInsns, insnMap)

//    println("allFrames")
//    allFrames.map(println)
    // force in-order registration of method arguments in first block

    val blockBuffers = computeBlockBuffers(clsName, blockMap, blockInsns, blockLines, allFrames)

//    if (false && method.name == "ensureObj") {
//      println(s"--------------------- Converting ${method.name} ---------------------")
//      for(i <- 0 until blockMap.length){
//        if (i == 0 || blockMap(i) != blockMap(i-1)) println("-------------- BasicBlock " + blockMap(i) + " --------------")
//        val insn = OPCODES.lift(allInsns(i).getOpcode).getOrElse(allInsns(i).getClass.getSimpleName).padTo(30, ' ')
//        val frame = Option(allFrames(blockMap(i))).map(_(insnMap(i)).toString).getOrElse("-")
//
//        println(lineMap(i) + "\t" + insn + " | " + blockMap(i) + " | " + insnMap(i) + " |\t" + frame)
//      }
//      println("XXX")
//    }

    val basicBlocks = computeBasicBlocks(method, blockBuffers)

    vm.logger.logBasicBlocks(clsName, method, basicBlocks, blockBuffers.map(_._3))

    val tryCatchBlocks = for(b <- Agg.from(method.tryCatchBlocks.asScala)) yield{
      TryCatchBlock(
        (b.start.block, b.start.insn),
        (b.end.block, b.end.insn),
        b.handler.block,
        blockBuffers(b.handler.block)._3(SingleInsnSSAConverter.top(allFrames(blockMap(allInsns.indexOf(b.handler))).head)),
        Option(b.`type`).map(imm.Type.Cls.apply)
      )
    }


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

    implicit def deref(label: LabelNode) = blockInsns.indexWhere(_.head == label)

    for {
      (blockFrames, blockId) <- Agg.from(allFrames).zipWithIndex
      if blockFrames != null
    } yield {

      val buffer = new Aggregator[Insn]

      val srcMap = new Aggregator[I]
      val lineMap = new Aggregator[I]
      val localMap = mutable.Map.empty[Box, I]
      var symCount = 0
      val types = new Aggregator[LocalType]


      implicit def getBox(b: Box) = {
        if (!localMap.contains(b)) {
          localMap(b) = symCount
          symCount += b.value.getSize
          import org.objectweb.asm.Type
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

      for ((insn, i) <- blockInsns(blockId).zipWithIndex) try {

        SingleInsnSSAConverter(
          insn,
          (in: Insn) => {
            buffer.append(in)
            srcMap.append(i)
            lineMap.append(blockLines(blockId)(i))
          },
          blockFrames(i),
          blockFrames(i + 1),
          blockMap
        )
      } catch {
        case e: Throwable =>
          println("ConvertInsn Failed " + clsName)
          throw e
      }

      (buffer, types, localMap, blockFrames.head, blockFrames.last, lineMap)
    }
  }

  def computeAllFrames(method: MethodNode,
                       allInsns: Array[AbstractInsnNode],
                       extraFrames: => Array[Frame[Box]],
                       blockMap: Array[I],
                       blockInsns: Array[Array[AbstractInsnNode]],
                       insnMap: Array[Int]) = {
    implicit class pimpedLabel(x: LabelNode){
      def block = blockMap(allInsns.indexOf(x))
      def insn = insnMap(allInsns.indexOf(x))
    }

    val links: Seq[(I, I)] = {
      val jumps =
        blockInsns.map(_.last)
          .zipWithIndex
          .flatMap {
            case (x: JumpInsnNode, i) => Seq(i -> x.label.block)
            case (x: TableSwitchInsnNode, i) => (x.dflt +: x.labels.asScala).map(i -> _.block)
            case (x: LookupSwitchInsnNode, i) => (x.dflt +: x.labels.asScala).map(i -> _.block)
            case _ => Nil
          }
      val flows =
        for {
          (a, b) <- (0 to blockMap.last - 1).zip(1 to blockMap.last)
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
      val catchFrame = extraFrames(allInsns.indexOf(b.handler))
      handle(catchFrame, blockInsns.indexWhere(_.head == b.handler))
    }
    existing

  }

  def computeBasicBlocks(method: MethodNode, blockBuffers:  Agg[(Agg[Insn], Agg[LocalType], mutable.Map[Box, Int], Frame[Box], Frame[Box], Agg[Int])]) = {
    for(((buffer, types, startMap, startFrame, _, lines), i) <- blockBuffers.zipWithIndex) yield {
      val phis = for(((buffer2, types2, endMap, _, endFrame, _), j) <- blockBuffers.zipWithIndex) yield {
        if (endFrame != null && startFrame != null && ((buffer2.length > 0 && buffer2.last.targets.contains(i)) || (i == j + 1))){
//          if (method.name == "initTable") {
//            println()
//            println("Making Phi       " + j + "->" + i)
//            println("endFrame         " + endFrame + "\t" + boxes(endFrame).flatten.map(endMap))
//            println("startFrame       " + startFrame + "\t" + boxes(startFrame).flatten.map(startMap))
//
//            println("endFrame.boxes   " + boxes(endFrame))
//            println("startFrame.boxes " + boxes(startFrame))
//            println("endMap           " + endMap)
//            println("startMap         " + startMap)
//            if(boxes(endFrame).length != boxes(startFrame).length) println(
//              fansi.Color.Red(
//                s"Start frame doesn't line up with End frame.\n" +
//                s"start frame $j has ${boxes(startFrame).length}, end frame $i has ${boxes(endFrame).length}"
//              )
//            )
//          }

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
      BasicBlock(buffer, phis/*.map(_.filter(x => x._1 != x._2))*/, types, lines)
    }
  }
}