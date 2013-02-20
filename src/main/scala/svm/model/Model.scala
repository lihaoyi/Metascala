package svm.model

import java.nio.ByteBuffer

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._

import org.objectweb.asm

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
    mn.instructions.toArray.toList.map(Node.read),
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
                  instructions: List[Node],
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



trait Node

object Node {
  def read(ain: AbstractInsnNode) = ain match{
    case x: FieldInsnNode => FieldInsn(x.getOpcode, x.owner, x.name, x.desc)
    case x: FrameNode => Frame(x.`type`, x.local.safeList, x.stack.safeList)
    case x: IincInsnNode => IIncInsn(x.getOpcode, x.`var`, x.incr)
    case x: InsnNode => Insn(x.getOpcode)
    case x: IntInsnNode => IntInsn(x.getOpcode, x.operand)
    case x: InvokeDynamicInsnNode => InvokeDynamicInsn(x.getOpcode, x.name, x.desc, x.bsm, x.bsmArgs)
    case x: JumpInsnNode => JumpInsn(x.getOpcode, x.label)
    case x: LabelNode => Label()
    case x: LdcInsnNode => LdcInsn(x.getOpcode, x.cst)
    case x: LineNumberNode => LineNumber(x.line, x.start)
    case x: LookupSwitchInsnNode => LookupSwitchInsn(x.getOpcode, x.dflt, x.keys.safeList.map(x => x: Int), x.labels.safeList)
    case x: MethodInsnNode => MethodInsn(x.getOpcode, x.owner, x.name, x.desc)
    case x: MultiANewArrayInsnNode => MultiANewArrayInsn(x.getOpcode, x.desc, x.dims)
    case x: TableSwitchInsnNode => TableSwitchInsn(x.getOpcode, x.min, x.max, x.dflt, x.labels.safeList)
    case x: TypeInsnNode => TypeInsn(x.getOpcode, x.desc)
    case x: VarInsnNode => VarInsn(x.getOpcode, x.`var`)
  }

  case class FieldInsn(opcode: Int,
                       owner: String,
                       name: String,
                       desc: String) extends Node

  case class Frame(frameType: Int,
                   local: List[Any],
                   stack: List[Any]) extends Node

  case class IIncInsn(opcode: Int,
                      variable: Int,
                      incr: Int) extends Node

  case class Insn(opcode: Int) extends Node

  case class IntInsn(opcode: Int,
                     operand: Int) extends Node

  case class InvokeDynamicInsn(opcode: Int,
                               name: String,
                               desc: String,
                               bsm: Any,
                               bsmArgs: Array[AnyRef]) extends Node


  case class JumpInsn(opcode: Int,
                      label: LabelNode) extends Node

  case class Label() extends Node

  case class LdcInsn(opcode: Int,
                     cst: Any) extends Node

  case class LineNumber(line: Int,
                        start: LabelNode) extends Node

  case class LookupSwitchInsn(opcode: Int,
                              default: LabelNode,
                              keys: List[Int],
                              labels: List[LabelNode]) extends Node

  case class MethodInsn(opcode: Int,
                        owner: String,
                        name: String,
                        desc: String) extends Node

  case class MultiANewArrayInsn(opcode: Int,
                                desc: String,
                                dims: Int) extends Node

  case class TableSwitchInsn(opcode: Int,
                             min: Int,
                             max: Int,
                             default: LabelNode,
                             labels: List[LabelNode]) extends Node

  case class TypeInsn(opcode: Int,
                      desc: String) extends Node

  case class VarInsn(opcode: Int,
                     variable: Int) extends Node

}