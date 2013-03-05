package svm

import model._
import collection.mutable
import java.io.{ObjectInputStream, ObjectOutputStream}
import annotation.tailrec
import scala.Some
import svm.ClsObj
import org.objectweb.asm.Type

class Cls(val classData: ClassData,
            val statics: mutable.Map[String, Any] = mutable.Map.empty)
           (implicit classes: String => Cls, loader: VClassLoader){

  classData.superName.map(classes)
  lazy val obj = new ClsObj(name)

  classData.fields.map{f =>
    statics(f.name) = Obj.initField(f.desc)
  }

  def method(name: String, desc: String): Option[Method] = {
    ancestry.flatMap(_.methods).find(m => m.name == name && m.desc == TypeDesc.read(desc))
  }

  def apply(owner: String, name: String) = {
    this.ancestry.dropWhile(_.name != owner)
                 .find(_.fields.exists(_.name == name))
                 .get.name.statics(name)
  }

  def update(owner: String, name: String, value: Any) = {
    this.ancestry.dropWhile(_.name != owner)
      .find(_.fields.exists(_.name == name))
      .get.name.statics(name) = value
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

  def isInstanceOf(desc: String)(implicit classes: String => Cls): Boolean = {

    val res =
      classData.name == desc ||
      classData.interfaces.contains(desc) ||
      (classData.superName.isDefined && classes(classData.superName.get).isInstanceOf(desc))

    res
  }
}

object Obj{

  def initMembers(cls: ClassData, filter: Field => Boolean)(implicit classes: String => Cls): List[Map[String, Any]] = {
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

class Obj(val cls: Cls, initMembers: (String, Any)*)
            (implicit classes: String => Cls){


  val members = Obj.initMembers(cls.classData, x => (x.access & Access.Static) == 0).map(x => mutable.Map(x.toSeq:_*))


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
    s"svm.Obj(${cls.name})"
  }
}

class ClsObj(val name: String)
                 (implicit classes: String => Cls)
                  extends Obj("java/lang/Class"){


  def getDeclaredConstructors() = {
    classes(name.replace(".", "/")).classData
      .methods
      .filter(_.name == "<init>")
      .map{m =>
      new svm.Obj("java/lang/reflect/Constructor",
        "clazz" -> classes(name.replace(".", "/")).obj,
        "slot" -> 0,
        "parameterTypes" -> m.desc.args.map(???),
        "exceptionTypes" -> new Array[svm.ClsObj](0),
        "modifiers" -> m.access
      )
    }
  }
  def getDeclaredFields() = {

    classes(name).classData.fields.map {f =>

      new svm.Obj("java/lang/reflect/Field",
        "clazz" -> this,
        "slot" -> f.name.hashCode,
        "name" -> Natives.intern(Virtualizer.toVirtual(f.name)),
        "modifiers" -> f.access,
        "type" -> classes(Type.getType(f.desc).getClassName).obj,
        "signature" -> Virtualizer.toVirtual(f.desc)

      )
    }
  }
  def getDeclaredMethods() = {

    /**
     * private Class<?>            clazz;
    private int                 slot;
    // This is guaranteed to be interned by the VM in the 1.4
    // reflection implementation
    private String              name;
    private Class<?>            returnType;
    private Class<?>[]          parameterTypes;
    private Class<?>[]          exceptionTypes;
    private int                 modifiers;
    // Generics and annotations support
    private transient String              signature;
    // generic info repository; lazily initialized
    private transient MethodRepository genericInfo;
    private byte[]              annotations;
    private byte[]              parameterAnnotations;
    private byte[]              annotationDefault;
    private volatile MethodAccessor methodAccessor;
    // For sharing of MethodAccessors. This branching structure is
    // currently only two levels deep (i.e., one root Method and
    // potentially many Method objects pointing to it.)
    private Method              root;
     */
    classes(name).classData.methods.map {m =>
      println(m.desc)
      new svm.Obj("java/lang/reflect/Method",
        "clazz" -> this,
        "slot" -> m.name.hashCode,
        "name" -> m.name,

        "modifiers" -> m.access,
        "returnType" -> ???,
        "parameterTypes" -> ???,
        "exceptionTypes" -> new Array[svm.ClsObj](0)

      )
    }
  }
  override def toString = {
    s"svm.ClsObj($name)"
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