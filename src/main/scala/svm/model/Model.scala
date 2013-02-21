package svm.model

import java.nio.ByteBuffer

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._

import org.objectweb.asm
import svm.model.opcodes.{Context, OpCodes, OpCode}
import collection.mutable

object InnerClass {
  def read(icn: InnerClassNode) = InnerClass(
    icn.name,
    icn.outerName,
    icn.innerName,
    icn.access
  )
}

case class InnerClass(name: String,
                      outerName: String,
                      innerName: String,
                      access: Int)

object LocalVariable {
  def read(lvn: LocalVariableNode) = LocalVariable(
    lvn.name,
    lvn.desc,
    lvn.signature,
    lvn.start,
    lvn.end,
    lvn.index
  )
}

case class LocalVariable(name: String,
                         desc: String,
                         signature: String,
                         start: LabelNode,
                         end: LabelNode,
                         index: Int)

object ClassFile {

  def read(cn: ClassNode) = ClassFile(
    cn.access,
    cn.name,
    cn.superName,
    cn.interfaces.safeList,
    cn.fields.safeList.map(Field.read),
    cn.methods.safeList.map(Method.read),
    Misc(
      cn.signature.safeOpt,
      cn.sourceFile.safeOpt,
      cn.sourceDebug.safeOpt,
      cn.outerClass.safeOpt,
      cn.outerMethod.safeOpt,
      cn.outerMethodDesc.safeOpt,
      cn.visibleAnnotations.safeList.map(Annotation.read),
      cn.invisibleAnnotations.safeList.map(Annotation.read),
      cn.attrs.safeList.map(Attribute.read),
      cn.innerClasses.safeList.map(InnerClass.read)
    )
  )

  def parse(input: Array[Byte]) = {
    val cr = new ClassReader(input);
    val classNode = new ClassNode();

    cr.accept(classNode, 0);
    read(classNode)
  }
  case class Misc(signature: Option[String],
                  sourceFile: Option[String],
                  sourceDebug: Option[String],
                  outerClass: Option[String],
                  outerMethod: Option[String],
                  outerMethodDesc: Option[String],
                  visibleAnnotations: List[Annotation],
                  invisibleAnnotations: List[Annotation],
                  attrs: List[Attribute],
                  innerClasses: List[InnerClass])
}

case class ClassFile(access_flags: Int,
                     name: String,
                     superName: String,
                     interfaces: List[String],
                     fields: List[Field],
                     methods: List[Method],
                     misc: ClassFile.Misc)


object Field {
  def read(fn: FieldNode) = Field(
    fn.access,
    fn.name,
    fn.desc,
    fn.signature.safeOpt,
    fn.value,
    fn.visibleAnnotations.safeList.map(Annotation.read),
    fn.invisibleAnnotations.safeList.map(Annotation.read),
    fn.attrs.safeList.map(Attribute.read)
  )

}

case class Field(access: Int,
                 name: String,
                 desc: String,
                 signature: Option[String],
                 value: Any,
                 visibleAnnotations: List[Annotation],
                 invisibleAnnotations: List[Annotation],
                 attrs: List[Attribute])

object Method {
  def read(mn: MethodNode) = Method(
    mn.access,
    mn.name,
    mn.desc,
    mn.exceptions.safeList,
    Code.read(mn.instructions),
    Misc(
      mn.signature.safeOpt,
      mn.tryCatchBlocks.safeList.map(TryCatchBlock.read),
      mn.localVariables.safeList.map(LocalVariable.read),
      mn.maxStack,
      mn.maxLocals,
      mn.attrs.safeList.map(Attribute.read)
    ),
    Annotations(
      mn.visibleAnnotations.safeList.map(Annotation.read),
      mn.invisibleAnnotations.safeList.map(Annotation.read),
      mn.annotationDefault.safeOpt,
      mn.visibleParameterAnnotations.safeList.map(_.safeList.map(Annotation.read)),
      mn.invisibleParameterAnnotations.safeList.map(_.safeList.map(Annotation.read))
    )
  )
  case class Annotations(visibleAnnotations: List[Annotation],
                         invisibleAnnotations: List[Annotation],
                         annotationDefault: Any,
                         visibleParameterAnnotations: List[List[Annotation]],
                         invisibleParameterAnnotations: List[List[Annotation]])
  case class Misc(signature: Option[String],
                  tryCatchBlocks: List[TryCatchBlock],
                  localVariables: List[LocalVariable],
                  maxStack: Int,
                  maxLocals: Int,
                  attrs: List[Attribute])
}
case class Method(access: Int,
                  name: String,
                  desc: String,
                  exceptions: List[String],
                  code: Code,
                  misc: Method.Misc,
                  annotations: Method.Annotations)

object TryCatchBlock {
  def read(tcbn: TryCatchBlockNode) = TryCatchBlock(
    tcbn.start,
    tcbn.end,
    tcbn.handler,
    tcbn.`type`
  )
}

case class TryCatchBlock(start: LabelNode,
                         end: LabelNode,
                         handler: LabelNode,
                         blockType: String)

object Attribute {
  def read(a: asm.Attribute) = Attribute(a.`type`)
}

case class Attribute(atype: String)

object Annotation {
  def read(an: AnnotationNode) = Annotation(
    an.desc,
    an.values.safeList
  )
}

case class Annotation(desc: String,
                      values: List[Any])


object Code{
  def read(nodes: InsnList): Code = {
    val instructions = mutable.ListBuffer[Node]()
    val attached =
      mutable.Map.empty[Int, mutable.ListBuffer[Attached]]
                 .withDefaultValue(mutable.ListBuffer())

    for(node <- nodes.toArray){
      node match{
        case Node(n) => instructions.append(n)
        case Attached(a) => attached(instructions.length).append(a)
      }
    }
    Code(instructions.toList, attached.toMap.mapValues(_.toSeq))
  }
}
case class Code(instructions: Seq[Node],
                attached: Map[Int, Seq[Attached]])

trait Attached
object Attached{
  case class Frame(frameType: Int,
                   local: List[Any],
                   stack: List[Any]) extends Attached
  case class Label() extends Attached
  case class LineNumber(line: Int,
                        start: LabelNode) extends Attached
  def unapply(ain: AbstractInsnNode) = read.lift(ain)
  def read: PartialFunction[AbstractInsnNode, Attached] = {
    case x: FrameNode               => Frame(x.`type`, x.local.safeList, x.stack.safeList)
    case x: LabelNode               => Label()
    case x: LineNumberNode          => LineNumber(x.line, x.start)
  }
}

trait Node{
  type This <: Node
  def opcode: OpCode[This]
  def op(ctx: Context) = opcode.op(ctx, this.asInstanceOf[This])
}

object Node {
  implicit class getOpCode(i: Int){
    def resolve[T <: Node] = OpCodes[T](i)
  }
  def unapply(ain: AbstractInsnNode) = read.lift(ain)
  def read: PartialFunction[AbstractInsnNode, Node] = {
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
                       desc: String) extends Node{
    type This = FieldInsn

  }



  case class IIncInsn(opcode: OpCode[IIncInsn],
                      variable: Int,
                      incr: Int) extends Node{
    type This = IIncInsn
  }

  case class Insn(opcode: OpCode[Insn]) extends Node{
    type This = Insn
  }

  case class IntInsn(opcode: OpCode[IntInsn],
                     operand: Int) extends Node{
    type This = IntInsn
  }

  case class InvokeDynamicInsn(opcode: OpCode[InvokeDynamicInsn],
                               name: String,
                               desc: String,
                               bsm: Any,
                               bsmArgs: Array[AnyRef]) extends Node{
    type This = InvokeDynamicInsn
  }


  case class JumpInsn(opcode: OpCode[JumpInsn],
                      label: LabelNode) extends Node{
    type This = JumpInsn
  }



  case class LdcInsn(opcode: OpCode[LdcInsn],
                     cst: Any) extends Node{
    type This = LdcInsn
  }


  case class LookupSwitchInsn(opcode: OpCode[LookupSwitchInsn],
                              default: LabelNode,
                              keys: List[Int],
                              labels: List[LabelNode]) extends Node{
    type This = LookupSwitchInsn
  }

  case class MethodInsn(opcode: OpCode[MethodInsn],
                        owner: String,
                        name: String,
                        desc: String) extends Node{
    type This = MethodInsn
  }

  case class MultiANewArrayInsn(opcode: OpCode[MultiANewArrayInsn],
                                desc: String,
                                dims: Int) extends Node{
    type This = MultiANewArrayInsn
  }

  case class TableSwitchInsn(opcode: OpCode[TableSwitchInsn],
                             min: Int,
                             max: Int,
                             default: LabelNode,
                             labels: List[LabelNode]) extends Node{
    type This = TableSwitchInsn
  }

  case class TypeInsn(opcode: OpCode[TypeInsn],
                      desc: String) extends Node{
    type This = TypeInsn
  }

  case class VarInsn(opcode: OpCode[VarInsn],
                     variable: Int) extends Node{
    type This = VarInsn
  }

}