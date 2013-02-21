package svm.model

import org.objectweb.asm.tree.MethodNode

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