package svm.model.opcodes


import svm.{Frame, VmThread, VirtualMachine}
import svm.model.Node.Insn
import svm.model.Node
import org.objectweb.asm.tree.LabelNode

case class Context(thread: VmThread){
  def classes = thread.classes
  def frame = thread.threadStack.head
  def stack = frame.stack

  def jumpTo(l: LabelNode) = ???
  def throwException(exception: Any) = ???
  def returnVal(x: Option[Any]) = {
    thread.threadStack.pop()
    x.foreach(value => thread.threadStack.head.stack = value :: thread.threadStack.head.stack)
  }
}

trait OpCode[-T <: Node]{
  def id: Byte
  def name: String
  def op: (Context, T) => Unit
}

object OpCode {
  def unapply[T <: Node](o: OpCode[T]) = (o.id, o.name, o.op)
  def apply[T <: Node](id: Byte, name: String)(op: (Context, T) => Unit) =
    BaseOpCode(id, name, op)

  case class BaseOpCode[T <: Node](id: Byte, name: String, op: (Context, T) => Unit) extends OpCode[T]



}

