package metascala
package imm

import org.objectweb.asm.tree._
import org.objectweb.asm.Label
import collection.mutable
import metascala.opcodes.OpCode

object Method {
  def read(mn: MethodNode) = {
    implicit val labelMap = Code.makeLabelMap(mn.instructions)
    Method(
      mn.access,
      mn.name,
      Desc.read(mn.desc),
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
}

case class Method(access: Int,
                  name: String,
                  desc: Desc,
                  exceptions: Seq[String] = Nil,
                  code: Code = Code(),
                  misc: Method.Misc = Method.Misc(),
                  annotations: Method.Annotations = Method.Annotations()){
  def concrete = code != Code()
  def static = (access & Access.Static) != 0
  lazy val sig = Sig(name, desc)
  def argSize = {
    val thisSize = if(static) 1 else 0
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
        case y if OpCode.read(Map.empty[Label, Int]).isDefinedAt(y) => i += 1
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

    val f = OpCode.read.andThen{o =>
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
  case class Frame(frameType: Int,
                   local: Seq[Any],
                   stack: Seq[Any]) extends Attached

  case class LineNumber(line: Int,
                        start: Int) extends Attached



  def read(implicit labelMap: Map[Label, Int]): PartialFunction[Any, Attached] = {
    case x: FrameNode       => Frame(x.`type`, x.local.safeSeq, x.stack.safeSeq)
    case x: LineNumberNode  => LineNumber(x.line, x.start.getLabel)

  }
}
