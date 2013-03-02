package svm

import model._
import collection.mutable
import java.io.{ObjectInputStream, ObjectOutputStream}
import annotation.tailrec
import scala.Some
import svm.ClassObject
import org.objectweb.asm.Type

class Class(val classData: ClassData,
            val statics: mutable.Map[String, Any] = mutable.Map.empty)
           (implicit classes: String => Class, loader: VClassLoader){

  System.out.println("NEW CLASS" + classData.name + "\t" + this.hashCode())
  lazy val obj = new ClassObject(name)
  classData.fields.map{f =>
    statics(f.name) = Object.initField(f.desc)
  }

  def method(name: String, desc: String): Option[Method] = {
    ancestry.flatMap(_.methods).find(m => m.name == name && m.desc == desc)
  }

  def name = classData.name

  def ancestry = {
    def rec(cd: ClassData): List[ClassData] = {
      cd.superName match{
        case None => List(cd)
        case Some(x) => cd :: rec(classes(x).classData)
      }
    }
    rec(classData)
  }

  def isInstanceOf(desc: String)(implicit classes: String => Class): Boolean = {

    val res =
      classData.name == desc ||
      classData.interfaces.contains(desc) ||
      (classData.superName.isDefined && classes(classData.superName.get).isInstanceOf(desc))

    res
  }
}

object Object{

  def initMembers(cls: ClassData, filter: Field => Boolean)(implicit classes: String => Class): List[Map[String, Any]] = {
    cls.fields.filter(filter).map{f =>
      f.name -> initField(f.desc)
    }.toMap :: cls.superName.toList.flatMap(x => initMembers(classes(x).classData, filter))
  }

  def initField(desc: String) = {
    desc(0) match{
      case 'B' => 0: Byte
      case 'C' => 0: Char
      case 'I' => 0
      case 'J' => 0L
      case 'F' => 0F
      case 'D' => 0D
      case 'S' => 0: Short
      case 'Z' => false
      case 'L' => null
      case '[' => null
    }
  }
}

class Object(val cls: Class, initMembers: (String, Any)*)
            (implicit classes: String => Class){


  val members = Object.initMembers(cls.classData, x => (x.access & Access.Static) == 0).map(x => mutable.Map(x.toSeq:_*))


  for ((s, v) <- initMembers){
    this(cls.name, s) = v
  }

  def apply(owner: String, name: String) = {
    val start = cls.ancestry.indexWhere(_.name == owner)

    members.drop(start)
           .find(_.contains(name))
           .get(name)

  }
  def update(owner: String, name: String, value: Any) = {
    val start = cls.ancestry.indexWhere(_.name == owner)

    members.drop(start)
           .find(_.contains(name))
           .get(name) = value

  }
  override def toString = {
    s"svm.Object(${cls.name})"
  }
}

class ClassObject(val name: String)
                 (implicit classes: String => Class)
                  extends Object("java/lang/Class"){


  System.out.println("NEW CLOBJECT" + name.name + "\t" + this.hashCode())
  def getDeclaredConstructors() = {
    classes(name.replace(".", "/")).classData
      .methods
      .filter(_.name == "<init>")
      .map{m =>
      new svm.Object("java/lang/reflect/Constructor",
        "clazz" -> classes(name.replace(".", "/")).obj,
        "slot" -> 0,
        "parameterTypes" -> Type.getType(m.desc).getArgumentTypes.map(_.getClassName).map(classes.andThen(_.obj)),
        "exceptionTypes" -> new Array[svm.ClassObject](0),
        "modifiers" -> m.access
      )
    }
  }
  def getDeclaredFields() = {

    classes(name).classData.fields.map {f =>

      new svm.Object("java/lang/reflect/Field",
        "clazz" -> this,
        "slot" -> 1,
        "name" -> Natives.intern(Virtualizer.toVirtual(f.name)),
        "modifiers" -> f.access,
        "type" -> classes(Type.getType(f.desc).getClassName).obj,
        "signature" -> Virtualizer.toVirtual(f.desc)

      )
    }
  }
  override def toString = {
    s"svm.ClassObject($name)"
  }
}
class ClassLoaderObject(implicit classes: String => Class)
                        extends Object("java/lang/ClassLoader")



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