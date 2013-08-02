package metascala
package opcodes

import metascala.imm.{Attached, Method}
import org.objectweb.asm.Type
import scala.collection.mutable
import imm.Type.Prim._
import org.objectweb.asm.tree._
import scala.collection.JavaConverters._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.Opcodes._

import org.objectweb.asm.util.Printer.OPCODES

class Box(val value: BasicValue) extends Value{
  override def getSize = value.getSize
  override def toString = Math.abs(hashCode).toString.take(2) + "" + value + " "

}
class AbstractFunnyInterpreter(b: Boolean) extends Interpreter[Box](ASM4){
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
  def merge(v1: Box, v2: Box) = if (b) v1 else v2
}
object IncrementalInterpreter extends AbstractFunnyInterpreter(false)
object FullInterpreter extends AbstractFunnyInterpreter(true)

object Conversion {
  val unconditionalJumps = Seq(GOTO, RETURN, ARETURN, IRETURN, FRETURN, LRETURN, DRETURN, ATHROW)


  def ssa(clsName: String, method: MethodNode)(implicit vm: VM): Code = {

    val allInsns = method.instructions.toArray
    lazy val extraFrames = new Analyzer(FullInterpreter).analyze(clsName, method)


    val blockMap: Array[Int] = {
      val jumps = allInsns.collect{case x: JumpInsnNode => x}
      val returns = Seq(RETURN, IRETURN, FRETURN, LRETURN, DRETURN, ARETURN, ATHROW)
      val exits = allInsns.filter(c => returns.contains(c.getOpcode))
      val lookupSwitches = allInsns.collect{case x: LookupSwitchInsnNode => x}
      val tableSwitches = allInsns.collect{case x: TableSwitchInsnNode => x}
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

      val map = new Array[Int](allInsns.length)
      for(i <- 1 until allInsns.length){
        val prev = map(i - 1)
        map(i) = prev max map(i)
        if (bases.contains(allInsns(i)) && i + 1 < map.length){
          map(i + 1) = prev + 1
        }else if (targets.contains(allInsns(i))){
          map(i) = prev + 1
        }
      }
      map
    }

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
              .toSeq
              .sortBy(_._1)
              .map(_._2)
              .map(_.map(_._2))
              .toArray
    }
    implicit def deref(label: LabelNode) = blockInsns.indexWhere(_.head == label)
//    println("blockInsns")
    blockInsns.map(_.map(_.getOpcode).filter(_!= -1).map(OPCODES).toSeq).foreach(println)
    val allFrames: Seq[Seq[Frame[Box]]] = {
      val links: Seq[(Int, Int)] = {
        val jumps =
          blockInsns.map(_.last)
            .zipWithIndex
            .flatMap{
            case (x: JumpInsnNode, i) => Seq(i -> x.label.block)
            case (x: TableSwitchInsnNode, i) => (x.dflt +: x.labels.asScala).map(i -> _.block)
            case (x: LookupSwitchInsnNode, i) => (x.dflt +: x.labels.asScala).map(i -> _.block)
            case _ => Nil
          }
        val flows =
          for{
            (a, b) <- (0 to blockMap.last - 1).zip(1 to blockMap.last)
            if !unconditionalJumps.contains(blockInsns(a).last.getOpcode)
          } yield (a, b)
//        println("jumps " + jumps.toSeq)
//        println("flows " + flows)
        jumps ++ flows
      }

//      println("links " + links)
      val existing = new Array[Seq[Frame[Box]]](blockInsns.length)
      def handle(startFrame: Frame[Box], blockId: Int): Unit = {
        if (existing(blockId) != null) ()
        else{
          val theseFrames = blockInsns(blockId).scanLeft(startFrame){ (frame, insn) =>
            val f = new Frame(frame)
            if (insn.getOpcode != -1) {
              f.execute(insn, IncrementalInterpreter)
            }
            f
          }
          existing(blockId) = theseFrames
          for{
            (src, dest) <- links
            if src == blockId
          } handle(theseFrames.last, dest)
        }
      }

      handle(extraFrames(0), 0)

      for(b <- method.tryCatchBlocks.asScala) {
        val catchFrame = extraFrames(allInsns.indexOf(b.handler))
        handle(catchFrame, b.handler)
      }
      existing
    }

//    println("allFrames")
//    allFrames.map(println)
    // force in-order registration of method arguments in first block

    val blockBuffers = for{
      (blockFrames, blockId) <- allFrames.zipWithIndex
      if blockFrames != null
    } yield {

      val buffer = mutable.Buffer.empty[Insn]

      val srcMap = mutable.Buffer.empty[Int]
      val localMap = mutable.Map.empty[Box, Int]
      var symCount = 0
      val types = mutable.Buffer.empty[imm.Type]


      implicit def getBox(b: Box) = {
        if (!localMap.contains(b)){
            localMap(b) = symCount
          symCount += b.value.getSize
          import org.objectweb.asm.Type
          assert (b != null)
          assert (b.value != null)
          assert (b.value.getType != null, "fail " + b.value)

          types.append(b.value.getType.getSort match{
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

      blockFrames(0).boxes.map(getBox)

      for ((insn, i) <- blockInsns(blockId).zipWithIndex){

        ConvertInsn(
          insn,
          (in: Insn) => {
            buffer.append(in)
            srcMap.append(i)
          },
          blockFrames(i),
          blockFrames(i+1),
          blockMap
        )
      }

      (buffer, types, localMap, blockFrames.head, blockFrames.last)

    }

    if (method.name == "arrayCasts") {
      for(i <- 0 until blockMap.length){
        if (i == 0 || blockMap(i) != blockMap(i-1)) println("-------------- BasicBlock " + blockMap(i) + " --------------")
        val insn = OPCODES.lift(allInsns(i).getOpcode).getOrElse(allInsns(i).getClass.getSimpleName).padTo(30, ' ')
        val frame = Option(allFrames(blockMap(i))).map(_(insnMap(i)).toString).getOrElse("-")
        println(insn + " | " + blockMap(i) + " | " + insnMap(i) + "\t" + frame)

      }
      println("XXX")
      println(allFrames.length)
      allFrames.filter(_ != null).map(_.length).map(println)
    }

//    println("++++++++++++++++++++++++++++++++++++++++")

    val basicBlocks = for(((buffer, types, startMap, startFrame, _), i) <- blockBuffers.zipWithIndex) yield {
      val phis = for(((buffer2, types2, endMap, _, endFrame), j) <- blockBuffers.zipWithIndex) yield {
        if (endFrame != null && startFrame != null && ((buffer2.length > 0 && buffer2.last.targets.contains(i)) || (i == j + 1))){
//          println()
//          println("Making Phi       " + j + "->" + i)
//          println("endFrame         " + endFrame + "\t" + endFrame.boxes.map(endMap))
//          println("startFrame       " + startFrame + "\t" + startFrame.boxes.map(startMap))
//          println("endFrame.boxes   " + endFrame.boxes)
//          println("startFrame.boxes " + startFrame.boxes)
//          println("endMap           " + endMap)
//          println("startMap         " + startMap)
          val zipped = for{
            (e, s) <- endFrame.boxes zip startFrame.boxes
            a <- endMap.get(e).toSeq
            b <- startMap.get(s).toSeq

            i <- 0 until e.getSize
          } yield (a + i, b + i)
//          println("zipped             " + zipped)
          zipped
        }else Nil

      }
      BasicBlock(buffer, phis, types)
    }

    if (method.name == "arrayCasts") {
      for ((block, i) <- basicBlocks.zipWithIndex){
        println()
        println(i + "\t" + block.phi.toList)
        println(i + "\t" + block.locals)
        block.insns.foreach(println)
        println(blockBuffers(i)._3)
      }
      println()
      println()
    }



    val tryCatchBlocks = for(b <- method.tryCatchBlocks.asScala) yield{
      TryCatchBlock(
        (b.start.block, b.start.insn),
        (b.end.block, b.end.insn),
        b.handler.block,
        blockBuffers(b.handler.block)._3(allFrames(blockMap(allInsns.indexOf(b.handler))).head.top()),
        Option(b.`type`).map(imm.Type.Cls.read)
      )
    }
//    println("============TryCatchBlocks===============")
//    tryCatchBlocks.map(println)

    Code(basicBlocks, tryCatchBlocks)
  }

}