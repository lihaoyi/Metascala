package metascala
package opcodes

import metascala.StackOps.OpCode
import metascala.imm.Type.Prim
import metascala.imm.{TryCatchBlock, Type}
import metascala.imm

class Jump(override val targets: Seq[Int]) extends Insn
class Invoke extends Insn

case class Code(blocks: Seq[BasicBlock] = Nil,
                tryCatches: Seq[TryCatchBlock] = Nil){
  lazy val localSize = blocks.map(_.locals.map(_.size).sum).max
}
case class BasicBlock(insns: Seq[Insn],
                      phi: Seq[Seq[(Sym, Sym)]],
                      locals: Seq[imm.Type])

case class TryCatchBlock(start: (Sym, Sym),
                         end: (Sym, Sym),
                         handler: Int,
                         destReg: Int,
                         blockType: Option[Type.Cls])

case class Step(insn: Insn, line: Int)

sealed trait Insn{
  def targets: Seq[Int] = Nil
}
object Insn{
  case class BinOp[A, B, R](a: Sym, pa: Prim[A], b: Sym, pb: Prim[B], out: Sym, pout: Prim[R], func: (A, B) => R) extends Insn
  case class UnaryOp[A, R](a: Sym, pa: Prim[A], out: Sym, pout: Prim[R], func: A => R) extends Insn
  case class UnaryBranch(a: Sym, target: Int, func: Int => Boolean) extends Jump(Seq(target))
  case class BinaryBranch(a: Sym, b: Sym, target: Int, func: (Int, Int) => Boolean) extends Jump(Seq(target))
  case class ReturnVal(a: Sym) extends Insn
  case class TableSwitch(src: Sym, min: Int, max: Int, default: Int, targetList: Seq[Int]) extends Jump(targetList :+ default)
  case class LookupSwitch(src: Sym, default: Int, keys: Seq[Int], targetList: Seq[Int]) extends Jump(targetList :+ default)
  case class Goto(target: Int) extends Jump(Seq(target))
  case class Ldc(target: Int, thing: Any) extends Insn
  case class Push[T](prim: Prim[T], target: Int, value: T) extends Insn
  case class InvokeStatic(target: Sym, sources: Seq[Sym], owner: Type.Cls, method: rt.Method) extends Invoke
  case class InvokeVirtual(target: Sym, sources: Seq[Sym], owner: Type.Cls, sig: imm.Sig, methodIndex: Int) extends Invoke
  case class InvokeInterface(target: Sym, sources: Seq[Sym], owner: Type.Cls, sig: imm.Sig) extends Invoke
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
