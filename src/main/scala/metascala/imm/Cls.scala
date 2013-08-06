package metascala.imm

import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.ClassReader
import NullSafe._
object Cls {

  def read(cn: ClassNode) = {
    Cls(
      cn.access,
      Type.Cls.read(cn.name),
      cn.superName.safeOpt.map(Type.Cls.read),
      cn.interfaces.safeSeq.map(Type.Cls.read),
      cn.fields.safeSeq.map(Field.read),
      cn.methods.safeSeq.map(Method.read),
      Misc(
        cn.signature.safeOpt,
        cn.sourceFile.safeOpt,
        cn.sourceDebug.safeOpt,
        cn.outerClass.safeOpt,
        cn.outerMethod.safeOpt,
        cn.outerMethodDesc.safeOpt,
        cn.visibleAnnotations.safeSeq.map(Annotation.read),
        cn.invisibleAnnotations.safeSeq.map(Annotation.read),
        cn.attrs.safeSeq.map(Attribute.read),
        cn.innerClasses.safeSeq.map(InnerClass.read)
      )
    )
  }

  def parse(input: Array[Byte]) = {
    val cr = new ClassReader(input)
    val classNode = new ClassNode()
    cr.accept(classNode, ClassReader.EXPAND_FRAMES)
    read(classNode)
  }

  case class Misc(signature: Option[String] = None,
                  sourceFile: Option[String] = None,
                  sourceDebug: Option[String] = None,
                  outerClass: Option[String] = None,
                  outerMethod: Option[String] = None,
                  outerMethodDesc: Option[String] = None,
                  visibleAnnotations: Seq[Annotation] = Nil,
                  invisibleAnnotations: Seq[Annotation] = Nil,
                  attrs: Seq[Attribute] = Nil,
                  innerClasses: Seq[InnerClass] = Nil)
}
case class Cls(access_flags: Int,
               tpe: Type.Cls,
               superType: Option[Type.Cls] = None,
               interfaces: Seq[Type.Cls] = Nil,
               fields: Seq[Field] = Nil,
               methods: Seq[Method] = Nil,
               misc: Cls.Misc = Cls.Misc())
