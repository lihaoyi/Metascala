package metascala.rt

import metascala.opcodes.{BasicBlock, Box, Insn}
import metascala.rt
import metascala.util.Agg
import org.objectweb.asm.tree.MethodNode

import scala.collection.mutable

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
  def logPhi(indentCount: Int, clsName: String, frame: Frame, shifts: Iterator[(Int, Int)]): Unit
  def logBasicBlocks(clsName: String,
                     method: MethodNode,
                     basicBlocks: TraversableOnce[BasicBlock],
                     blockBufferThrees: Agg[mutable.Map[Box, Int]]): Unit
  def logException(ex: Throwable): Unit
}
object NonLogger extends Logger{
  def active = false
  def logStep(indentCount: Int,
              clsName: String,
              frame: Frame,
              node: Insn,
              block: BasicBlock): Unit = ()
  def logPhi(indentCount: Int, clsName: String, frame: Frame, shifts: Iterator[(Int, Int)]): Unit = ()
  def logBasicBlocks(clsName: String,
                     method: MethodNode,
                     basicBlocks: TraversableOnce[BasicBlock],
                     blockBufferThrees: Agg[mutable.Map[Box, Int]]): Unit = ()
  def logException(ex: Throwable) = ()
}