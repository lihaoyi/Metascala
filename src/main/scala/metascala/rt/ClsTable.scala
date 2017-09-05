package metascala.rt

import metascala.imm.{Sig, Type}
import metascala.opcodes.MethodSSAConverter
import metascala.{imm, opcodes, rt, util}
import metascala.util.{NullSafe, Ref, WritableRef}
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode

import scala.collection.mutable

object ClsTable{
  case class ClsNotFound(tpe: imm.Type.Cls) extends Exception("Can't find " + tpe.javaName)
}

case class PatchedConstantBox(addr: WritableRef)

/**
  * Cache of all the classes loaded so far within the Metascala VM.
  */
class ClsTable(fileLoader: String => Option[Array[Byte]])
              (implicit vm: opcodes.SingleInsnSSAConverter.VMInterface) extends ClsTable0 {
  val clsIndex = mutable.ArrayBuffer[rt.Cls](null)
  val cache = mutable.Map.empty[imm.Type.Cls, rt.Cls]
  def apply(x: imm.Type.Cls) = {
    cache.get(x) match {
      case Some(y) => y
      case None =>
        val newY = calc(x)
        cache(x) = newY
        clsIndex.append(newY)
        newY
    }
  }
  def calc(t: imm.Type.Cls): rt.Cls = {
    val fileName = t.javaName.replace('.', '/') + ".class"
    val input = fileLoader(fileName).getOrElse(throw ClsTable.ClsNotFound(t))

    calcFromBytes0(input, null)
  }

  def calcFromBytes(x: imm.Type.Cls, input: Array[Byte], cpPatches: Map[Int, Int] = null): rt.Cls = {
    cache.get(x) match{
      case Some(y) => y
      case None =>
        val newY = calcFromBytes0(input, cpPatches)
        cache(x) = newY
        clsIndex.append(newY)
        newY
    }
  }
  def calcFromBytes0(input: Array[Byte], cpPatches: Map[Int, Int]): rt.Cls = {
    val classNode = new ClassNode()
    val cr = new ClassReader(input){
      override def readConst(item: Int, buf: Array[Char]): AnyRef ={
        if (cpPatches != null && cpPatches.contains(item)){
          PatchedConstantBox(new util.Ref.UnsafeManual(cpPatches(item)))
        }else{
          super.readConst(item, buf)
        }
      }
    }
    cr.accept(classNode, ClassReader.EXPAND_FRAMES)

    Option(classNode.superName).map(Type.Cls.apply).map(apply)
    val fields = NullSafe(classNode.fields).map(imm.Field.read)
    val superType = NullSafe(classNode.superName).map(Type.Cls.apply)
    new Cls(
      tpe = imm.Type.Cls.apply(classNode.name),
      superType = superType,
      sourceFile = NullSafe(classNode.sourceFile),
      interfaces = NullSafe(classNode.interfaces).map(Type.Cls.apply),
      accessFlags = classNode.access,
      methods =
        NullSafe(classNode.methods)
          .zipWithIndex
          .map{case (mn, i) =>
            new rt.ClsMethod(
              clsIndex.length,
              i,
              Sig(mn.name, imm.Desc.read(mn.desc)),
              mn.access,
              () => MethodSSAConverter.apply(classNode.name, mn)
            )
          },
      fieldList =
        superType.toSeq.flatMap(apply(_).fieldList) ++
          fields.filter(!_.static).flatMap{x =>
            Seq.fill(x.desc.size)(x)
          },
      staticList =
        fields.filter(_.static).flatMap{x =>
          Seq.fill(x.desc.size)(x)
        },
      outerCls = NullSafe(classNode.outerClass).map(Type.Cls.apply),
      clsIndex.length
    )
  }
  var startTime = System.currentTimeMillis()

}

