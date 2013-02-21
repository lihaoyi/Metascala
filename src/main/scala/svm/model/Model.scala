package svm.model

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree._

import org.objectweb.asm
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


object Code{
  def read(nodes: InsnList): Code = {
    val instructions = mutable.ListBuffer[OpCode]()
    val attached =
      mutable.Map.empty[Int, List[Attached]]
                 .withDefaultValue(Nil)

    for(node <- nodes.toArray){
      node match{
        case OpCode.TryParse(n) =>
          println("Instruction " + n)
          instructions.append(n)
        case Attached.TryParse(a) =>
          println("Attached " + a)
          println(attached)
          attached(instructions.length) = a :: attached(instructions.length)
          println(attached)
      }
    }
    val result = Code(instructions.toList, attached.toMap)
    println("Result")
    println(attached)
    println(result)
    result
  }

}
case class Code(instructions: Seq[OpCode],
                attached: Map[Int, Seq[Attached]])


