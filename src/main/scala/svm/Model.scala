package svm

import model._
import collection.mutable
import java.io.{ObjectInputStream, ObjectOutputStream}
import annotation.tailrec
import scala.Some


class Cls(val classData: ClassData,
          val statics: mutable.Map[String, Any] = mutable.Map.empty)
         (implicit classes: Type.Cls => Cls){

  implicit def autoClass(x: String) = classes(svm.model.Type.Cls(x))
  classData.superType.map(classes)
  lazy val obj = new ClsObj(Type.Cls(name))

  classData.fields.map{f =>
    statics(f.name) = Obj.initField(f.desc)
  }

  def method(name: String, desc: Type.Desc): Option[Method] = {
    ancestry.flatMap(_.methods).find(m => m.name == name && m.desc == desc)
  }

  def apply(owner: Type.Cls, name: String) = {
    this.ancestry.dropWhile(_.tpe != owner)
                 .find(_.fields.exists(_.name == name))
                 .get.tpe.statics(name)
  }

  def update(owner: Type.Cls, name: String, value: Any) = {
    println(

    )
    this.ancestry.dropWhile(_.tpe != owner)
      .find(_.fields.exists(_.name == name))
      .get.tpe.statics(name) = value
  }

  def name = classData.tpe.name

  def ancestry = {
    def rec(cd: ClassData): List[ClassData] = {
      cd.superType match{
        case None => List(cd)
        case Some(x) => cd :: rec(x.classData)
      }
    }
    rec(classData)
  }

  def isInstanceOf(desc: Type)(implicit classes: Type.Cls => Cls): Boolean = {

    val res =
      classData.tpe == desc ||
      classData.interfaces.contains(desc) ||
      (classData.superType.isDefined && classes(classData.superType.get).isInstanceOf(desc))

    res
  }
}

object Obj{

  def initMembers(cls: ClassData, filter: Field => Boolean)(implicit classes: Type.Cls => Cls): List[Map[String, Any]] = {
    cls.fields.filter(filter).map{f =>
      f.name -> initField(f.desc)
    }.toMap :: cls.superType.toList.flatMap(x => initMembers(classes(x).classData, filter))
  }

  def initField(desc: Type) = {

    desc match{
      case Type.Prim("B") => 0: Byte
      case Type.Prim("C") => 0: Char
      case Type.Prim("I") => 0
      case Type.Prim("J") => 0L
      case Type.Prim("F") => 0F
      case Type.Prim("D") => 0D
      case Type.Prim("S") => 0: Short
      case Type.Prim("Z") => false
      case _ => null

    }
  }
  def apply(clsName: String, initMembers: (String, Any)*)(implicit classes: Type.Cls => Cls) = {
    new Obj(Type.Cls(clsName), initMembers: _*)
  }
}
class Obj(val cls: Cls, initMembers: (String, Any)*)
            (implicit classes: Type.Cls => Cls){


  val members = Obj.initMembers(cls.classData, x => (x.access & Access.Static) == 0).map(x => mutable.Map(x.toSeq:_*))


  for ((s, v) <- initMembers){
    this(Type.Cls.read(cls.name), s) = v
  }

  def apply(owner: Type.Cls, name: String) = {
    val start = cls.ancestry.indexWhere(_.tpe == owner)

    members.drop(start)
           .find(_.contains(name))
           .get(name)

  }
  def update(owner: Type.Cls, name: String, value: Any) = {
    val start = cls.ancestry.indexWhere(_.tpe == owner)

    members.drop(start)
           .find(_.contains(name))
           .get(name) = value

  }
  override def toString = {
    s"svm.Obj(${cls.name})"
  }
}
object TpeObj{
  def apply(t: Type)(implicit classes: Type.Cls => Cls) = t match{
    case tpe: Type.Cls => new ClsObj(tpe)
    case tpe => new TpeObj(tpe)
  }
}
class TpeObj(val tpe: Type)(implicit classes: Type.Cls => Cls)
  extends Obj(Type.Cls("java/lang/Class")){
  def getDeclaredConstructors() = new Array[Object](0)
  def getDeclaredFields() = new Array[Object](0)
  def getDeclaredMethods() = new Array[Object](0)

}
class ClsObj(val tpe: Type.Cls)
            (implicit classes: Type.Cls => Cls)
             extends Obj(Type.Cls("java/lang/Class")){

  def name = tpe.unparse
  def getDeclaredConstructors() = {
    classes(tpe).classData
      .methods
      .filter(_.name == "<init>")
      .map{m =>
      svm.Obj("java/lang/reflect/Constructor",
        "clazz" -> tpe.obj,
        "slot" -> 0,
        "parameterTypes" -> m.desc.args.map(???),
        "exceptionTypes" -> new Array[svm.ClsObj](0),
        "modifiers" -> m.access
      )
    }
  }
  def getDeclaredFields() = {
      classes(tpe).classData.fields.map {f =>

        svm.Obj("java/lang/reflect/Field",
          "clazz" -> this,
          "slot" -> f.name.hashCode,
          "name" -> Natives.intern(Virtualizer.toVirtual(f.name)),
          "modifiers" -> f.access,
          "type" -> f.desc,
          "signature" -> Virtualizer.toVirtual(f.desc)

        )
      }
  }
  def getDeclaredMethods() = {

    classes(tpe).classData.methods.map {m =>
      println(m.desc)
      svm.Obj("java/lang/reflect/Method",
        "clazz" -> this,
        "slot" -> m.name.hashCode,
        "name" -> m.name,

        "modifiers" -> m.access,
        "returnType" -> m.desc.ret,
        "parameterTypes" -> ???,
        "exceptionTypes" -> new Array[svm.ClsObj](0)

      )
    }
  }
  override def toString = {
    s"svm.ClsObj(${tpe.unparse}})"
  }
}



object Access{

  val Public    = 0x0001 // 1
  val Private   = 0x0002 // 2
  val Protected = 0x0004 // 4
  val Static    = 0x0008 // 8
  val Final     = 0x0010 // 16
  val Super     = 0x0020 // 32
  val Volatile  = 0x0040 // 64
  val Transient = 0x0080 // 128
  val Native    = 0x0100 // 256
  val Interface = 0x0200 // 512
  val Abstract  = 0x0400 // 1024
  val Strict    = 0x0800 // 2048
}