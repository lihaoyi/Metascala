package metascala
package imm
import Type.Prim._
import org.objectweb.asm.tree._
import org.objectweb.asm.Label
import collection.mutable
import NullSafe._
object Method {
  def read(mn: MethodNode) = {
    Method(
      mn,
      mn.access,
      Sig(
        mn.name,
        Desc.read(mn.desc)
      ),
      mn.exceptions.safeSeq
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
  def shortName = {
    val some :+ last = name.split("/").toSeq
    (some.map(_(0)) :+ last).mkString("/")
  }
}

case class Method(mn: MethodNode,
                  access: Int,
                  sig: Sig,
                  exceptions: Seq[String] = Nil){

  def static = (access & Access.Static) != 0
  def name = sig.name
  def desc = sig.desc
  def argSize = {
    val thisSize = if(static) 0 else 1
    thisSize + desc.argSize
  }
}
