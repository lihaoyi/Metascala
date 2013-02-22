package svm.model

import org.objectweb.asm.tree._


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

object Code{
  val Empty = Code(Nil, Nil)

  def read(nodesList: InsnList) = {
    val code: Code = {
      val nodes = nodesList.toArray

      val labelMapMaker = collection.mutable.Map.empty[Int, Int]
      var i = 0
      for(node <- nodes){
        node match{
          case x: LabelNode => labelMapMaker(x.getLabel.hashCode()) = i
          case y if OpCode.read(Map.empty[Int, Int]).isDefinedAt(y) => i += 1
          case _ => ()
        }
      }
      implicit val labelMap = labelMapMaker.toMap
      var instructions: List[OpCode] = Nil
      var allAttached: List[List[Attached]] = Nil
      var attached: List[Attached] = Nil
      for(node <- nodes){
        node match{
          case OpCode.TryParse(n) =>
            instructions ::= n
            allAttached ::= attached
            attached = Nil
          case Attached.TryParse(a) =>
            attached ::= a
          case x: LabelNode => ()
        }
      }
      Code(instructions.reverse, allAttached.reverse)
    }
    code
  }
}

case class Code(instructions: List[OpCode],
                attachments: List[List[Attached]])

trait Attached
object Attached{
  case class Frame(frameType: Int,
                   local: List[Any],
                   stack: List[Any]) extends Attached

  case class LineNumber(line: Int,
                        start: Int) extends Attached

  object TryParse{
    def unapply(x: AbstractInsnNode)(implicit labelMap: Map[Int, Int]) = read.lift(x)
  }

  def read(implicit labelMap: Map[Int, Int]): PartialFunction[Any, Attached] = {
    case x: FrameNode       => Frame(x.`type`, x.local.safeList, x.stack.safeList)
    case x: LineNumberNode  => LineNumber(x.line, labelMap(x.start.getLabel.hashCode()))
  }
}
