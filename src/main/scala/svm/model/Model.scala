package svm.model

import org.objectweb.asm.tree._

import org.objectweb.asm
import asm.{Type, Label}
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

object Field {
  def read(fn: FieldNode) = {

    Field(
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

}

case class Field(access: Int,
                 name: String,
                 desc: String,
                 signature: Option[String],
                 value: Any,
                 visibleAnnotations: List[Annotation],
                 invisibleAnnotations: List[Annotation],
                 attrs: List[Attribute])




object TryCatchBlock {
  def read(tcbn: TryCatchBlockNode)(implicit labelMap: Map[Label, Int]) = TryCatchBlock(
    tcbn.start.getLabel,
    tcbn.end.getLabel,
    tcbn.handler.getLabel,
    tcbn.`type`.safeOpt
  )
}

case class TryCatchBlock(start: Int,
                         end: Int,
                         handler: Int,
                         blockType: Option[String])

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




