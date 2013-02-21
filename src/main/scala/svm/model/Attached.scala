package svm.model

import org.objectweb.asm.tree.{LineNumberNode, FrameNode, AbstractInsnNode, LabelNode}

trait Attached
object Attached{
  case class Frame(frameType: Int,
                   local: List[Any],
                   stack: List[Any]) extends Attached
  case class Label() extends Attached
  case class LineNumber(line: Int,
                        start: LabelNode) extends Attached

  object TryParse{
    def unapply(x: AbstractInsnNode) = read.lift(x)
  }

  def read: PartialFunction[AbstractInsnNode, Attached] = {
    case x: FrameNode               => Frame(x.`type`, x.local.safeList, x.stack.safeList)
    case x: LabelNode               => Label()
    case x: LineNumberNode          => LineNumber(x.line, x.start)
  }
}
