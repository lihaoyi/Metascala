package svm.model

import svm.model.opcodes.{OpCodes, Context, OpCode}
import org.objectweb.asm.tree._

object Instruction {

  implicit class getOpCode(i: Int){
    def resolve[T <: Instruction] = OpCodes[T](i)
  }
  def unapply(ain: AbstractInsnNode) = read.lift(ain)
  def read: PartialFunction[AbstractInsnNode, Instruction] = {
    case x: FieldInsnNode           => FieldInsn(x.getOpcode.resolve, x.owner, x.name, x.desc)
    case x: IincInsnNode            => IIncInsn(x.getOpcode.resolve, x.`var`, x.incr)
    case x: InsnNode                => Insn(x.getOpcode.resolve)
    case x: IntInsnNode             => IntInsn(x.getOpcode.resolve, x.operand)
    case x: InvokeDynamicInsnNode   => InvokeDynamicInsn(x.getOpcode.resolve, x.name, x.desc, x.bsm, x.bsmArgs)
    case x: JumpInsnNode            => JumpInsn(x.getOpcode.resolve, x.label)
    case x: LdcInsnNode             => LdcInsn(x.getOpcode.resolve, x.cst)
    case x: LookupSwitchInsnNode    => LookupSwitchInsn(x.getOpcode.resolve, x.dflt, x.keys.safeList.map(x => x: Int), x.labels.safeList)
    case x: MethodInsnNode          => MethodInsn(x.getOpcode.resolve, x.owner, x.name, x.desc)
    case x: MultiANewArrayInsnNode  => MultiANewArrayInsn(x.getOpcode.resolve, x.desc, x.dims)
    case x: TableSwitchInsnNode     => TableSwitchInsn(x.getOpcode.resolve, x.min, x.max, x.dflt, x.labels.safeList)
    case x: TypeInsnNode            => TypeInsn(x.getOpcode.resolve, x.desc)
    case x: VarInsnNode             => VarInsn(x.getOpcode.resolve, x.`var`)
  }

  case class FieldInsn(opcode: OpCode[FieldInsn],
                       owner: String,
                       name: String,
                       desc: String) extends Instruction{
    type This = FieldInsn

  }



  case class IIncInsn(opcode: OpCode[IIncInsn],
                      variable: Int,
                      incr: Int) extends Instruction{
    type This = IIncInsn
  }

  case class Insn(opcode: OpCode[Insn]) extends Instruction{
    type This = Insn
  }

  case class IntInsn(opcode: OpCode[IntInsn],
                     operand: Int) extends Instruction{
    type This = IntInsn
  }

  case class InvokeDynamicInsn(opcode: OpCode[InvokeDynamicInsn],
                               name: String,
                               desc: String,
                               bsm: Any,
                               bsmArgs: Array[AnyRef]) extends Instruction{
    type This = InvokeDynamicInsn
  }


  case class JumpInsn(opcode: OpCode[JumpInsn],
                      label: LabelNode) extends Instruction{
    type This = JumpInsn
  }



  case class LdcInsn(opcode: OpCode[LdcInsn],
                     cst: Any) extends Instruction{
    type This = LdcInsn
  }


  case class LookupSwitchInsn(opcode: OpCode[LookupSwitchInsn],
                              default: LabelNode,
                              keys: List[Int],
                              labels: List[LabelNode]) extends Instruction{
    type This = LookupSwitchInsn
  }

  case class MethodInsn(opcode: OpCode[MethodInsn],
                        owner: String,
                        name: String,
                        desc: String) extends Instruction{
    type This = MethodInsn
  }

  case class MultiANewArrayInsn(opcode: OpCode[MultiANewArrayInsn],
                                desc: String,
                                dims: Int) extends Instruction{
    type This = MultiANewArrayInsn
  }

  case class TableSwitchInsn(opcode: OpCode[TableSwitchInsn],
                             min: Int,
                             max: Int,
                             default: LabelNode,
                             labels: List[LabelNode]) extends Instruction{
    type This = TableSwitchInsn
  }

  case class TypeInsn(opcode: OpCode[TypeInsn],
                      desc: String) extends Instruction{
    type This = TypeInsn
  }

  case class VarInsn(opcode: OpCode[VarInsn],
                     variable: Int) extends Instruction{
    type This = VarInsn
  }

}
trait Instruction{
  type This <: Instruction
  def opcode: OpCode[This]
  def op(ctx: Context) = opcode.op(ctx, this.asInstanceOf[This])
}

