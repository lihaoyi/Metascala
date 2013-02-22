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
  def read(nodes: InsnList) = {
    lazy val code: Code = {
      implicit val codeGetter = () => code
      var instructions: List[OpCode] = Nil
      var allAttached: List[List[Attached]] = Nil
      var attached: List[Attached] = Nil

      for(node <- nodes.toArray){

        node match{
          case OpCode.TryParse(n) =>
            instructions ::= n
            allAttached ::= attached
            attached = Nil
          case Attached.TryParse(a) =>
            attached ::= a
        }
      }
      Code(instructions.reverse, allAttached.reverse)
    }
    code
  }
}

case class LazyLabel(label: LabelNode)(implicit code: () => Code){
  lazy val labelFound = ???
  def apply() = labelFound
}
case class Code(instructions: List[OpCode],
                attachments: List[List[Attached]])

trait Attached
object Attached{
  case class Frame(frameType: Int,
                   local: List[Any],
                   stack: List[Any]) extends Attached
  case class Label() extends Attached
  case class LineNumber(line: Int,
                        start: LabelNode) extends Attached

  object TryParse{
    def unapply(x: AbstractInsnNode)(implicit code: () => Code) = read.lift(x)
  }

  def read(implicit code: () => Code): PartialFunction[Any, Attached] = {
    case x: FrameNode       => Frame(x.`type`, x.local.safeList, x.stack.safeList)
    case x: LabelNode       => Label()
    case x: LineNumberNode  => LineNumber(x.line, x.start)
  }
}
