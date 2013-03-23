package sm.imm

import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.ClassReader

object Cls {

  def read(cn: ClassNode) = {
    Cls(
      cn.access,
      Type.Cls.read(cn.name),
      cn.superName.safeOpt.map(Type.Cls.read),
      cn.interfaces.safeList.map(Type.Cls.read),
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
  }

  def parse(input: Array[Byte]) = {
    val cr = new ClassReader(input)
    val classNode = new ClassNode()
    cr.accept(classNode, 0)
    read(classNode)
  }

  case class Misc(signature: Option[String] = None,
                  sourceFile: Option[String] = None,
                  sourceDebug: Option[String] = None,
                  outerClass: Option[String] = None,
                  outerMethod: Option[String] = None,
                  outerMethodDesc: Option[String] = None,
                  visibleAnnotations: List[Annotation] = Nil,
                  invisibleAnnotations: List[Annotation] = Nil,
                  attrs: List[Attribute] = Nil,
                  innerClasses: List[InnerClass] = Nil)
}
case class Cls(access_flags: Int,
               tpe: Type.Cls,
               superType: Option[Type.Cls] = None,
               interfaces: List[Type.Cls] = Nil,
               fields: List[Field] = Nil,
               methods: List[Method] = Nil,
               misc: Cls.Misc = Cls.Misc())
