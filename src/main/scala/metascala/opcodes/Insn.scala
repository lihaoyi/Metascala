package metascala
package opcodes


import metascala.imm.Type.Prim
import metascala.imm.Type
import metascala.util.Agg

class Jump(override val targets: Agg[Int]) extends Insn
class Invoke extends Insn

case class Code(blocks: Agg[BasicBlock] = Agg.empty,
                tryCatches: Agg[TryCatchBlock] = Agg.empty){
  lazy val localSize = blocks.map(_.locals.map(_.size).sum).max
}
sealed class LocalType(val size: Int, toString: String, val prettyRead: (() => Int) => String)
object LocalType{
  val Int = new LocalType(1, "I", "I#" + imm.Type.Prim.I.read(_).toString)
  val Long = new LocalType(2, "J", "J#" + imm.Type.Prim.J.read(_).toString)
  val Float = new LocalType(1, "F", "F#" + imm.Type.Prim.F.read(_).toString)
  val Double = new LocalType(2, "D", "D#" + imm.Type.Prim.D.read(_).toString)
  val Ref = new LocalType(1, "L", "L#" + _().toString)
}
case class BasicBlock(insns: Agg[Insn],
                      phi: Agg[Agg[Int]],
                      locals: Agg[LocalType],
                      lines: Agg[Int])

case class TryCatchBlock(start: (Int, Int),
                         end: (Int, Int),
                         handler: Int,
                         destReg: Int,
                         blockType: Option[Type.Cls])

case class Step(insn: Insn, line: Int)

sealed trait Insn{
  def targets: Agg[Int] = Agg.empty
}
object Insn{
  case class BinOp[A, B, R](a: Int, pa: Prim[A], b: Int, pb: Prim[B], out: Int, pout: Prim[R], func: (A, B) => R) extends Insn
  case class UnaryOp[A, R](a: Int, pa: Prim[A], out: Int, pout: Prim[R], func: A => R) extends Insn

  case class UnaryBranch(a: Int, target: Int, func: Int => Boolean) extends Jump(Agg(target))
  case class BinaryBranch(a: Int, b: Int, target: Int, func: (Int, Int) => Boolean) extends Jump(Agg(target))
  case class ReturnVal(a: Int) extends Insn
  case class AThrow(src: Int) extends Insn

  case class TableSwitch(src: Int, min: Int, max: Int, default: Int, targetList: Agg[Int]) extends Jump(targetList ++ Agg(default))
  case class LookupSwitch(src: Int, default: Int, keys: Agg[Int], targetList: Agg[Int]) extends Jump(targetList ++ Agg(default))
  case class Goto(target: Int) extends Jump(Agg(target))

  case class CheckCast(src: Int, dest: Int, desc: Type) extends Insn
  case class ArrayLength(src: Int, dest: Int) extends Insn
  case class InstanceOf(src: Int, dest: Int, desc: Type) extends Insn

  case class Push[T](dest: Int, prim: Prim[T], value: T) extends Insn
  case class Ldc(dest: Int, value: Int) extends Insn

  case class InvokeStatic(dest: Int, srcs: Agg[Int], clsIndex: Int, methodIndex: Int, special: Boolean) extends Invoke
  case class InvokeVirtual(dest: Int, srcs: Agg[Int], clsIndex: Int, methodIndex: Int) extends Invoke
  case class InvokeInterface(dest: Int, srcs: Agg[Int], sig: imm.Sig) extends Invoke
  case class InvokeHandle(dest: Int, srcs: Agg[Int], sig: imm.Sig) extends Invoke
  case class InvokeDynamic(target: Int,
                           srcs: Agg[Int],
                           name: String,
                           desc: String,
                           bsTag: Int,
                           bsOwner: String,
                           bsName: String,
                           bsDesc: String,
                           bsArgs: Seq[AnyRef]) extends Invoke

  case class New(target: Int, clsIndex: Int) extends Insn
  case class NewArray(src: Int, dest: Int, typeRef: imm.Type) extends Insn
  case class MultiANewArray(desc: Type, target: Int, dims: Seq[Int]) extends Insn

  // offsets fixed/fixed
  case class PutStatic(src: Int, clsIndex: Int, index: Int, prim: Type) extends Insn
  case class GetStatic(dest: Int, clsIndex: Int, index: Int, prim: Type) extends Insn

  // offsets relative/fixed
  case class PutField(src: Int, obj: Int, index: Int, prim: Type) extends Insn
  case class GetField(dest: Int, obj: Int, index: Int, prim: Type) extends Insn

  // offsets relative/relative
  case class PutArray(src: Int, indexSrc: Int, array: Int, prim: Type) extends Insn
  case class GetArray(dest: Int, indexSrc: Int, array: Int, prim: Type) extends Insn
}
