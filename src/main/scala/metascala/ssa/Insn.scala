package metascala
package ssa

import metascala.opcodes.{StackManip, OpCode}
import metascala.imm.Type
import metascala.imm

trait Jump{
  def phi: Seq[(Symbol[_], Symbol[_])]
  def target: Int
}

trait Insn
object Insn{
  case class BinOp[A, B, R](a: Symbol[A], b: Symbol[B], out: Symbol[R], src: StackManip.BinOp[A, B, R]) extends Insn
  case class UnaryOp[A, R](a: Symbol[A], out: Symbol[R], src: StackManip.UnaryOp[A, R]) extends Insn
  case class UnaryBranch[A](a: Symbol[A], target: Int, src: OpCode, phi: Seq[(Symbol[_], Symbol[_])] = Nil) extends Insn with Jump
  case class BinaryBranch[A, B](a: Symbol[A], b: Symbol[B], target: Int, src: OpCode, phi: Seq[(Symbol[_], Symbol[_])] = Nil) extends Insn with Jump
  case class ReturnVal(a: Seq[Symbol[_]]) extends Insn
  case class Goto(target: Int, phi: Seq[(Symbol[_], Symbol[_])] = Nil) extends Insn with Jump
  case class Ldc(target: Int, thing: Any) extends Insn
  case class Push[T](prim: Prim[T], target: Int, value: T) extends Insn
  case class InvokeStatic(target: Symbol[_], sources: Seq[Symbol[_]], owner: Type.Cls, sig: imm.Sig) extends Insn
  case class New(cls: rt.Cls) extends Insn
}
