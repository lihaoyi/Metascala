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
  case class BinOp[A, B, R](a: Symbol[A], b: Symbol[B], out: Symbol[R], src: opcodes.BinOp[A, B, R]) extends Insn
  case class UnaryOp[A, R](a: Symbol[A], out: Symbol[R], src: opcodes.UnaryOp[A, R]) extends Insn
  case class UnaryBranch[A](a: Symbol[A], target: Int, src: opcodes.UnaryBranch, phi: Seq[(Symbol[_], Symbol[_])] = Nil) extends Insn with Jump
  case class BinaryBranch[A, B](a: Symbol[A], b: Symbol[B], target: Int, src: opcodes.BinaryBranch, phi: Seq[(Symbol[_], Symbol[_])] = Nil) extends Insn with Jump
  case class ReturnVal(a: Symbol[_]) extends Insn
  case class Goto(target: Int, phi: Seq[(Symbol[_], Symbol[_])] = Nil) extends Insn with Jump
  case class Ldc(target: Int, thing: Any) extends Insn
  case class Push[T](prim: Prim[T], target: Int, value: T) extends Insn
  case class InvokeStatic(target: Symbol[_], sources: Seq[Symbol[_]], owner: Type.Cls, sig: imm.Sig) extends Insn
  case class InvokeVirtual(target: Symbol[_], sources: Seq[Symbol[_]], owner: Type.Cls, sig: imm.Sig) extends Insn
  case class New(target: Symbol[_], cls: rt.Cls) extends Insn
  case class PutStatic(src: Symbol[_], cls: rt.Cls, index: Int, prim: Prim[_]) extends Insn
  case class GetStatic(src: Symbol[_], cls: rt.Cls, index: Int, prim: Prim[_]) extends Insn
  case class PutField(src: Symbol[_], obj: Symbol[_], index: Int, prim: Prim[_]) extends Insn
  case class GetField(src: Symbol[_], obj: Symbol[_], index: Int, prim: Prim[_]) extends Insn
  case class NewArray(src: Symbol[_], dest: Symbol[_], typeRef: imm.Type) extends Insn
  case class StoreArray[T](src: Symbol[_], index: Symbol[_], array: Symbol[_], prim: Prim[T]) extends Insn
}
