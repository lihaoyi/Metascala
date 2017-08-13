package metascala
package opcodes


import metascala.imm.Type.Prim
import metascala.imm.Type
import metascala.imm

class Jump(override val targets: Agg[Int]) extends Insn
class Invoke extends Insn

case class Code(blocks: Agg[BasicBlock] = Agg.empty,
                tryCatches: Agg[TryCatchBlock] = Agg.empty){
  lazy val localSize = blocks.map(_.locals.map(_.size).sum).max
}
case class BasicBlock(insns: Agg[Insn],
                      phi: Agg[Agg[(Sym, Sym)]],
                      locals: Agg[imm.Type],
                      lines: Agg[Int])

case class TryCatchBlock(start: (Sym, Sym),
                         end: (Sym, Sym),
                         handler: Int,
                         destReg: Int,
                         blockType: Option[Type.Cls])

case class Step(insn: Insn, line: Int)

sealed trait Insn{
  def targets: Agg[Int] = Agg.empty
}
object Insn{
  case class BinOp[A, B, R](a: Sym, pa: Prim[A], b: Sym, pb: Prim[B], out: Sym, pout: Prim[R], func: (A, B) => R) extends Insn
  case class UnaryOp[A, R](a: Sym, pa: Prim[A], out: Sym, pout: Prim[R], func: A => R) extends Insn

  case class UnaryBranch(a: Sym, target: Int, func: Int => Boolean) extends Jump(Agg(target))
  case class BinaryBranch(a: Sym, b: Sym, target: Int, func: (Int, Int) => Boolean) extends Jump(Agg(target))
  case class ReturnVal(a: Sym) extends Insn
  case class AThrow(src: Sym) extends Insn

  case class TableSwitch(src: Sym, min: Int, max: Int, default: Int, targetList: Agg[Int]) extends Jump(targetList ++ Agg(default))
  case class LookupSwitch(src: Sym, default: Int, keys: Agg[Int], targetList: Agg[Int]) extends Jump(targetList ++ Agg(default))
  case class Goto(target: Int) extends Jump(Agg(target))

  case class CheckCast(src: Sym, dest: Sym, desc: Type) extends Insn
  case class ArrayLength(src: Sym, dest: Sym) extends Insn
  case class InstanceOf(src: Sym, dest: Sym, desc: Type) extends Insn

  case class Push[T](dest: Sym, prim: Prim[T], value: T) extends Insn
  case class Ldc(dest: Sym, value: Int) extends Insn

  case class InvokeStatic(dest: Sym, srcs: Agg[Sym], owner: Type.Cls, methodIndex: Int, special: Boolean) extends Invoke
  case class InvokeVirtual(dest: Sym, srcs: Agg[Sym], owner: Type.Cls, sig: imm.Sig, methodIndex: Int) extends Invoke
//  case class InvokeDynamic(name: String, desc: String, bsm: String, bsmArgs: Seq[Sym]) extends Invoke

  case class New(target: Sym, cls: rt.Cls) extends Insn
  case class NewArray(src: Sym, dest: Sym, typeRef: imm.Type) extends Insn
  case class MultiANewArray(desc: Type, target: Sym, dims: Seq[Sym]) extends Insn

  // offsets fixed/fixed
  case class PutStatic(src: Sym, cls: rt.Cls, index: Int, prim: Type) extends Insn
  case class GetStatic(dest: Sym, cls: rt.Cls, index: Int, prim: Type) extends Insn

  // offsets relative/fixed
  case class PutField(src: Sym, obj: Sym, index: Int, prim: Type) extends Insn
  case class GetField(dest: Sym, obj: Sym, index: Int, prim: Type) extends Insn

  // offsets relative/relative
  case class PutArray(src: Sym, indexSrc: Sym, array: Sym, prim: Type) extends Insn
  case class GetArray(dest: Sym, indexSrc: Sym, array: Sym, prim: Type) extends Insn
}
