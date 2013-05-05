package metascala
package opcodes
import metascala.imm.Type
import collection.mutable
import rt.Thread

trait  Misc {
  case class Goto(label: Int) extends OpCode

  // These guys are meant to be deprecated in java 6 and 7
  //===============================================================
  val Ret = UnusedOpCode
  val Jsr = UnusedOpCode
  //===============================================================

  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends OpCode

  case class LookupSwitch(defaultTarget: Int, keys: Seq[Int], targets: Seq[Int]) extends OpCode
  case class ReturnVal(n: Int) extends OpCode

  val IReturn = ReturnVal(1)
  val LReturn = ReturnVal(2)
  val FReturn = ReturnVal(1)
  val DReturn = ReturnVal(2)
  val AReturn = ReturnVal(1)
  val Return = ReturnVal(0)

  case class GetStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode
  case class PutStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode

  case class GetField(owner: Type.Cls, name: String, desc: Type) extends OpCode
  case class PutField(owner: Type.Cls, name: String, desc: Type) extends OpCode

  case class InvokeVirtual(owner: Type.Ref, sig: imm.Sig) extends OpCode



  case class InvokeSpecial(owner: Type.Cls, sig: imm.Sig) extends OpCode
  case class InvokeStatic(owner: Type.Cls, sig: imm.Sig) extends OpCode
  case class InvokeInterface(owner: Type.Cls, sig: imm.Sig) extends OpCode
  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends OpCode

  case class New(desc: Type.Cls) extends OpCode

  case class NewArray(typeCode: Int) extends OpCode
  case class ANewArray(desc: imm.Type.Ref) extends OpCode

  case object ArrayLength extends OpCode
  case object AThrow extends OpCode
  case class CheckCast(desc: Type) extends OpCode

  case class InstanceOf(desc: Type) extends OpCode
  case object MonitorEnter extends OpCode
  case object MonitorExit extends OpCode

  // Not used, because ASM folds these into the following bytecode for us
  //===============================================================
  val Wide = UnusedOpCode
  //===============================================================

  case class MultiANewArray(desc: Type.Arr, dims: Int) extends OpCode
  val IfNull = opcodes.UnaryBranch(_: Int)(_ == 0)("IfNull")
  val IfNonNull = opcodes.UnaryBranch(_: Int)(_ != 0)("IfNull")

  // Not used, because ASM converts these to normal Goto()s and Jsr()s
  //===============================================================
  val GotoW = UnusedOpCode
  val JsrW = UnusedOpCode
  //===============================================================

}
