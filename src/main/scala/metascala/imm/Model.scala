package metascala
package imm

import org.objectweb.asm.tree._

import org.objectweb.asm
import asm.Label
import collection.mutable
import NullSafe._

object Field {
  def read(fn: FieldNode) = {

    Field(
      fn.access,
      fn.name,
      imm.Type.read(fn.desc),
      fn.signature.safeOpt,
      fn.value
    )
  }

}

case class Field(access: Int,
                 name: String,
                 desc: metascala.imm.Type,
                 signature: Option[String],
                 value: Any){
  def static = (access & Access.Static) != 0
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
