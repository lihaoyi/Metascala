package metascala.ssa

import metascala.opcodes.OpCode
import metascala.imm.Type
import metascala.imm

trait Jump{
  def phi: Seq[(Symbol, Symbol)]
  def target: Int
}

trait Insn
object Insn{
  case class BinOp(a: Symbol, b: Symbol, out: Symbol, src: OpCode) extends Insn
  case class UnaryOp(a: Symbol, out: Symbol, src: OpCode) extends Insn
  case class UnaryBranch(a: Symbol, target: Int, src: OpCode, phi: Seq[(Symbol, Symbol)] = Nil) extends Insn with Jump
  case class BinaryBranch(a: Symbol, b: Symbol, target: Int, src: OpCode, phi: Seq[(Symbol, Symbol)] = Nil) extends Insn with Jump
  case class ReturnVal(a: Symbol, n: Int) extends Insn
  case class Goto(target: Int, phi: Seq[(Symbol, Symbol)] = Nil) extends Insn with Jump

  case class InvokeStatic(target: Symbol, sources: Seq[Symbol], owner: Type.Cls, sig: imm.Sig) extends Insn



}
