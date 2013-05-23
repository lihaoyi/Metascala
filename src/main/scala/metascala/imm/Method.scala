package metascala
package imm

import org.objectweb.asm.tree._
import org.objectweb.asm.Label
import collection.mutable
import metascala.StackOps.OpCode

object Method {
  def read(mn: MethodNode) = {
    implicit val labelMap = Code.makeLabelMap(mn.instructions)
    Method(
      mn.access,
      Sig(
        mn.name,
        Desc.read(mn.desc)
      ),
      mn.exceptions.safeSeq,
      Code.read(mn.instructions),
      Misc(
        mn.signature.safeOpt,
        mn.tryCatchBlocks.safeSeq.map(TryCatchBlock.read),
        mn.localVariables.safeSeq.map(LocalVariable.read),
        mn.maxStack,
        mn.maxLocals,
        mn.attrs.safeSeq.map(Attribute.read)
      ),
      Annotations(
        mn.visibleAnnotations.safeSeq.map(Annotation.read),
        mn.invisibleAnnotations.safeSeq.map(Annotation.read),
        mn.annotationDefault.safeOpt,
        mn.visibleParameterAnnotations.safeSeq.map(_.safeSeq.map(Annotation.read)),
        mn.invisibleParameterAnnotations.safeSeq.map(_.safeSeq.map(Annotation.read))
      )
    )
  }

  case class Annotations(visibleAnnotations: Seq[Annotation] = Nil,
                         invisibleAnnotations: Seq[Annotation] = Nil,
                         annotationDefault: Any = null,
                         visibleParameterAnnotations: Seq[Seq[Annotation]] = Nil,
                         invisibleParameterAnnotations: Seq[Seq[Annotation]] = Nil)
  case class Misc(signature: Option[String] = None,
                  tryCatchBlocks: Seq[TryCatchBlock] = Nil,
                  localVariables: Seq[LocalVariable] = Nil,
                  maxStack: Int = 0,
                  maxLocals: Int = 0,
                  attrs: Seq[Attribute] = Nil)


}
case class Sig(name: String, desc: Desc){
  override lazy val hashCode = name.hashCode + desc.hashCode
  def unparse = name + desc.unparse
  override def toString = unparse
}

case class Method(access: Int,
                  sig: Sig,
                  exceptions: Seq[String] = Nil,
                  code: Code = Code(),
                  misc: Method.Misc = Method.Misc(),
                  annotations: Method.Annotations = Method.Annotations()){
  def concrete = code != Code()
  def static = (access & Access.Static) != 0
  def name = sig.name
  def desc = sig.desc
  def argSize = {
    val thisSize = if(static) 0 else 1
    thisSize + desc.argSize
  }
}

object Code{


  def makeLabelMap(nodesList: InsnList): Map[Label, Int] = {
    val nodes = nodesList.toArray

    val labelMapMaker = collection.mutable.Map.empty[Label, Int]
    var i = 0
    for(node <- nodes){
      node match{
        case x: LabelNode => labelMapMaker(x.getLabel) = i
        case y if StackOps.read(Map.empty[Label, Int]).isDefinedAt(y) => i += 1
        case _ => ()
      }
    }
    labelMapMaker.toMap
  }
  def read(nodesList: InsnList)(implicit labelMap: Map[Label, Int]) = {

    val nodes = nodesList.toArray
    val instructions = mutable.ArrayBuffer.empty[OpCode]
    val allAttached = mutable.ArrayBuffer.empty[List[Attached]]
    var attached: List[Attached] = Nil

    val f = StackOps.read.andThen{o =>
      instructions += o
      allAttached += attached

      attached = Nil
    } orElse Attached.read.andThen{ a =>
      attached ::= a
    }

    for(node <- nodes){
      f.lift(node)
    }

    Code(instructions, allAttached)
  }
}

case class Code(insns: Seq[OpCode] = Nil,
                attachments: Seq[Seq[Attached]] = Nil)

trait Attached
object Attached{
  object Frame{
    object Type{
      def read(x: AnyRef) = x match{
        case s: String => Class(s)
        case x: java.lang.Integer => all(x)
      }
    }
    class Type(val size: Int)
    case class Class(v: String) extends Type(1)
    case object Top extends Type(1)
    case object Integer extends Type(1)
    case object Float extends Type(1)
    case object Double extends Type(2)
    case object Long extends Type(1)
    case object Null extends Type(1)
    case object UninitializedThis extends Type(1)
    val all = Seq(
      Top,
      Integer,
      Float,
      Double,
      Long,
      Null,
      UninitializedThis
    )
  }
  case class Frame(stack: List[Attached.Frame.Type],
                   locals: Vector[Attached.Frame.Type]) extends Attached

  case class LineNumber(line: Int,
                        start: Int) extends Attached




  def read(implicit labelMap: Map[Label, Int]): PartialFunction[Any, Attached] = {
    case x: FrameNode       =>
      assert(x.`type` == -1)
      Frame(
        x.stack.safeSeq.map(Frame.Type.read).reverse.toList,
        x.local.safeSeq.map(Frame.Type.read).toVector
      )
    case x: LineNumberNode  => LineNumber(x.line, x.start.getLabel)
  }
}
