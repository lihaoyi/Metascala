package metascala.rt

import metascala.opcodes.{BasicBlock, Box, Insn, TryCatchBlock}
import metascala.{imm, rt}
import metascala.util.Agg
import org.objectweb.asm.tree.MethodNode

import scala.collection.mutable

/**
  * The stack frame created by every method call
  */
class Frame(var pc: (Int, Int) = (0, 0),
            val runningClass: rt.Cls,
            val method: rt.ClsMethod,
            val returnTo: Int => Unit,
            val locals: Array[Int],
            val indexOfFrameWhosePCToIncrement: Int)

trait Logger{
  def active: Boolean
  def logStep(indentCount: Int,
              clsName: String,
              frame: Frame,
              node: Insn,
              block: BasicBlock,
              getType: Int => String): Unit
  def logPhi(indentCount: Int, clsName: String, frame: Frame, shifts: Agg[Int]): Unit
  def logBasicBlocks(clsName: String,
                     methodName: String,
                     desc: String,
                     basicBlocks: TraversableOnce[BasicBlock],
                     blockBufferThrees: Agg[mutable.Map[Box, Int]],
                     tryCatchBlocks: Agg[TryCatchBlock]): Unit
  def logException(cls: imm.Type.Cls, msg: String, frames: Seq[StackTraceElement]): Unit
}

object NonLogger extends Logger{
  def active = false
  def logStep(indentCount: Int,
              clsName: String,
              frame: Frame,
              node: Insn,
              block: BasicBlock,
              getType: Int => String): Unit = ()
  def logPhi(indentCount: Int, clsName: String, frame: Frame, shifts: Agg[Int]): Unit = ()
  def logBasicBlocks(clsName: String,
                     methodName: String,
                     desc: String,
                     basicBlocks: TraversableOnce[BasicBlock],
                     blockBufferThrees: Agg[mutable.Map[Box, Int]],
                     tryCatchBlocks: Agg[TryCatchBlock]): Unit = ()
  def logException(cls: imm.Type.Cls, msg: String, frames: Seq[StackTraceElement]) = ()
}