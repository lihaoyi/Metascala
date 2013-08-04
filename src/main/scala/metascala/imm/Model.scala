package metascala
package imm

import org.objectweb.asm.tree._

import org.objectweb.asm
import asm.Label
import collection.mutable
import NullSafe._
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
      imm.Type.read(fn.desc),
      fn.signature.safeOpt,
      fn.value,
      fn.visibleAnnotations.safeSeq.map(Annotation.read),
      fn.invisibleAnnotations.safeSeq.map(Annotation.read),
      fn.attrs.safeSeq.map(Attribute.read)
    )
  }

}

case class Field(access: Int,
                 name: String,
                 desc: metascala.imm.Type,
                 signature: Option[String],
                 value: Any,
                 visibleAnnotations: Seq[Annotation],
                 invisibleAnnotations: Seq[Annotation],
                 attrs: Seq[Attribute]){
  def static = (access & Access.Static) != 0
}




object TryCatchBlock {
  def read(tcbn: TryCatchBlockNode)(implicit labelMap: Map[Label, Int]) = TryCatchBlock(
    tcbn.start.getLabel,
    tcbn.end.getLabel,
    tcbn.handler.getLabel,
    tcbn.`type`.safeOpt.map(Type.Cls.read)
  )
}

case class TryCatchBlock(start: Int,
                         end: Int,
                         handler: Int,
                         blockType: Option[Type.Cls])

object Attribute {
  def read(a: asm.Attribute) = Attribute(a.`type`)
}

case class Attribute(atype: String)

object Annotation {
  def read(an: AnnotationNode) = Annotation(
    an.desc,
    an.values.safeSeq
  )
}

case class Annotation(desc: String,
                      values: Seq[Any])




