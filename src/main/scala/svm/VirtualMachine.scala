package svm

/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 10/13/12
 * Time: 11:47 PM
 * To change this template use File | Settings | File Templates.
 */
class VirtualMachine{
  val threads = List[Thread]()
  val heap = ???
  val methodArea = ???
}
case class VMThread(
  var pc: Int = 0,
  var stack: Seq[Frame] = Seq()
)

case class Frame(
  locals: Seq[Any],
  operandStack: Seq[Any],
  runtimeConstantPool: Any
)

