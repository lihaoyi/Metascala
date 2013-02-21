package svm.model

import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.ClassReader

object ClassFile {

  def read(cn: ClassNode) = ClassFile(
    cn.access,
    cn.name,
    cn.superName,
    cn.interfaces.safeList,
    cn.fields.safeList.map(Field.read),
    cn.methods.safeList.map(Method.read),
    Misc(
      cn.signature.safeOpt,
      cn.sourceFile.safeOpt,
      cn.sourceDebug.safeOpt,
      cn.outerClass.safeOpt,
      cn.outerMethod.safeOpt,
      cn.outerMethodDesc.safeOpt,
      cn.visibleAnnotations.safeList.map(Annotation.read),
      cn.invisibleAnnotations.safeList.map(Annotation.read),
      cn.attrs.safeList.map(Attribute.read),
      cn.innerClasses.safeList.map(InnerClass.read)
    )
  )

  def parse(input: Array[Byte]) = {
    val cr = new ClassReader(input);
    val classNode = new ClassNode();

    cr.accept(classNode, 0);
    read(classNode)
  }
  case class Misc(signature: Option[String],
                  sourceFile: Option[String],
                  sourceDebug: Option[String],
                  outerClass: Option[String],
                  outerMethod: Option[String],
                  outerMethodDesc: Option[String],
                  visibleAnnotations: List[Annotation],
                  invisibleAnnotations: List[Annotation],
                  attrs: List[Attribute],
                  innerClasses: List[InnerClass])
}
case class ClassFile(access_flags: Int,
                     name: String,
                     superName: String,
                     interfaces: List[String],
                     fields: List[Field],
                     methods: List[Method],
                     misc: ClassFile.Misc)
