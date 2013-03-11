package sm.virt

import sm.imm
import sm.{Virtualizer, virt, VM}
import sm.virt.Obj


object Type{
  def apply(t: imm.Type)(implicit vm: VM) = t match{
    case tpe: imm.Type.Cls => new Cls(tpe)
    case tpe => new Type(tpe)
  }
}
class Type(val tpe: imm.Type)(implicit vm: VM)
  extends Obj(vm.Classes(imm.Type.Cls("java/lang/Class"))){
  def getDeclaredConstructors() = new Array[Object](0)
  def getDeclaredFields() = new Array[Object](0)
  def getDeclaredMethods() = new Array[Object](0)
  def getInterfaces() = new Array[Object](0)
  override def toString = {
    s"virt.Type(${tpe.unparse})"
  }
}
class Cls(override val tpe: imm.Type.Cls)
            (implicit vm: VM)
             extends Type(tpe){
  import vm._
  def name = tpe.unparse
  override def getDeclaredConstructors() = {
    tpe.clsData
      .methods
      .filter(_.name == "<init>")
      .map{m =>
      virt.Obj("java/lang/reflect/Constructor",
        "clazz" -> tpe.obj,
        "slot" -> 0,
        "parameterTypes" -> m.desc.args.map(_.obj),
        "exceptionTypes" -> new Array[virt.Cls](0),
        "modifiers" -> m.access
      )
    }.toArray
  }
  override def getDeclaredFields() = {
      tpe.clsData.fields.map {f =>

        virt.Obj("java/lang/reflect/Field",
          "clazz" -> this,
          "slot" -> f.name.hashCode,
          "name" -> vm.InternedStrings(Virtualizer.toVirtual(f.name)),
          "modifiers" -> f.access,
          "type" -> f.desc.obj

        )
      }.toArray
  }

  override def getDeclaredMethods() = {

    tpe.clsData.methods.map {m =>
      virt.Obj("java/lang/reflect/Method",
        "clazz" -> this,
        "slot" -> m.name.hashCode,
        "name" -> vm.InternedStrings(Virtualizer.toVirtual(m.name)),

        "modifiers" -> m.access,
        "returnType" -> m.desc.ret.obj,
        "parameterTypes" -> m.desc.args.map(_.obj).toArray,
        "exceptionTypes" -> new Array[virt.Cls](0)

      )
    }.toArray
  }
  override def getInterfaces() = {
    tpe.clsData.interfaces.map(_.obj).toArray
  }
  override def toString = {
    s"virt.Cls(${tpe.unparse}})"
  }
}
