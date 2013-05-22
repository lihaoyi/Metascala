package metascala
package ssa

import metascala.opcodes.{OpCode}
import metascala.imm.Type
import metascala.imm

trait Jump{
  def phi: Seq[(Sym, Sym)]
  def target: Int
}



sealed trait Insn
object Insn{


  case class BinOp[A, B, R](a: Sym, b: Sym, out: Sym, src: opcodes.BinOp[A, B, R]) extends Insn
  case class UnaryOp[A, R](a: Sym, out: Sym, src: opcodes.UnaryOp[A, R]) extends Insn
  case class UnaryBranch[A](a: Sym, target: Int, src: opcodes.UnaryBranch, phi: Seq[(Sym, Sym)] = Nil) extends Insn with Jump
  case class BinaryBranch[A, B](a: Sym, b: Sym, target: Int, src: opcodes.BinaryBranch, phi: Seq[(Sym, Sym)] = Nil) extends Insn with Jump
  case class ReturnVal(a: Sym) extends Insn
  case class Goto(target: Int, phi: Seq[(Sym, Sym)] = Nil) extends Insn with Jump
  case class Ldc(target: Int, thing: Any) extends Insn
  case class Push[T](prim: Prim[T], target: Int, value: T) extends Insn
  case class InvokeStatic(target: Sym, sources: Seq[Sym], owner: Type.Cls, sig: imm.Sig) extends Insn
  case class InvokeSpecial(target: Sym, sources: Seq[Sym], owner: Type.Cls, sig: imm.Sig) extends Insn
  case class InvokeVirtual(target: Sym, sources: Seq[Sym], owner: Type.Cls, sig: imm.Sig) extends Insn
  case class New(target: Sym, cls: rt.Cls) extends Insn
  case class PutStatic(src: Sym, cls: rt.Cls, index: Int, prim: Prim[_]) extends Insn
  case class GetStatic(src: Sym, cls: rt.Cls, index: Int, prim: Prim[_]) extends Insn
  case class PutField(src: Sym, obj: Sym, index: Int, prim: Prim[_]) extends Insn
  case class GetField(src: Sym, obj: Sym, index: Int, prim: Prim[_]) extends Insn
  case class NewArray(src: Sym, dest: Sym, typeRef: imm.Type) extends Insn
  case class StoreArray[T](src: Sym, index: Sym, array: Sym, prim: Prim[T]) extends Insn
  case class LoadArray[T](dest: Sym, index: Sym, array: Sym, prim: Prim[T]) extends Insn
  case class AThrow(src: Sym) extends Insn
  case class CheckCast(src: Sym, desc: Type) extends Insn
  case class ArrayLength(src: Sym, dest: Sym) extends Insn
  case class InstanceOf(src: Sym, dest: Sym, desc: Type) extends Insn
  case class MultiANewArray(desc: Type, target: Sym, dims: Seq[Sym]) extends Insn
}
