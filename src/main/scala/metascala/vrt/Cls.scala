package metascala.vrt

import metascala.imm
import metascala.{vrt, VM}


object Type{
  def apply(t: imm.Type)(implicit vm: VM) = t match{
    case tpe: imm.Type.Cls => new Cls(tpe)
    case tpe => new Type(tpe)
  }
}
class Type(val heldType: imm.Type, initMembers: (String, vrt.Val)*)
          (implicit vm: VM)
          extends Obj(vm.ClsTable(imm.Type.Cls("java/lang/Class")), initMembers: _*){
  def getDeclaredConstructors() = Arr.Obj(imm.Type.Cls("java/lang/reflect/Constructor"), 0)
  def getDeclaredFields() = Arr.Obj(imm.Type.Cls("java/lang/reflect/Field"), 0)
  def getDeclaredMethods() = Arr.Obj(imm.Type.Cls("java/lang/reflect/Method"), 0)
  def getInterfaces() = new Array[vrt.Obj](0)
  override def toString = {
    s"vrt.Type(${heldType.unparse})"
  }
}
class Cls(override val heldType: imm.Type.Cls)
          (implicit vm: VM)
             extends Type(heldType, "name" -> heldType.name.replace('/', '.')){
  import vm._
  def name = heldType.unparse
  override def getDeclaredConstructors() = {
    val res = heldType.clsData
      .methods
      .filter(_.name == "<init>")
      .map{m =>
      vrt.Obj("java/lang/reflect/Constructor",
        "clazz" -> heldType.obj,
        "slot" -> 0,
        "parameterTypes" -> new vrt.Arr.Obj(imm.Type.Cls("java/lang/reflect/Type"), m.desc.args.map(_.obj).toArray),
        "exceptionTypes" -> new Array[vrt.Cls](0),
        "modifiers" -> m.access
      )
    }.toArray.map(x => x: vrt.Val)
    new vrt.Arr.Obj(imm.Type.Cls("java/lang/reflect/Constructor"), res)
  }

  override def getDeclaredFields() = {
    val res = heldType.clsData.fields.map {f =>

      vrt.Obj("java/lang/reflect/Field",
        "clazz" -> heldType.obj,
        "slot" -> f.name.hashCode,
        "name" -> vm.InternedStrings(f.name),
        "modifiers" -> f.access,
        "type" -> f.desc.obj

      )
    }.toArray.map(x => x: vrt.Val)
    new vrt.Arr.Obj(imm.Type.Cls("java/lang/reflect/Field"), res)

  }

  override def getDeclaredMethods() = {

    val res = heldType.clsData.methods.map {m =>
      vrt.Obj("java/lang/reflect/Method",
        "clazz" -> this,
        "slot" -> m.name.hashCode,
        "name" -> vm.InternedStrings(m.name),

        "modifiers" -> m.access,
        "returnType" -> m.desc.ret.obj,
        "parameterTypes" -> new vrt.Arr.Obj(imm.Type.Cls("java/lang/reflect/Type"), m.desc.args.map(_.obj).toArray),
        "exceptionTypes" -> new Array[vrt.Cls](0)

      )
    }.toArray.map(x => x: vrt.Val)
    new vrt.Arr.Obj(imm.Type.Cls("java/lang/reflect/Method"), res)
  }
  override def getInterfaces(): Array[vrt.Obj] = {
    heldType.clsData.interfaces.map(_.obj).toArray
  }
  override def toString = {
    s"vrt.Cls(${heldType.unparse}})"
  }
}
