package svm

import model.{Method, OpCode, ClassFile}
import collection.mutable
class Class(val classFile: ClassFile,
            val statics: mutable.Map[String, Any] = mutable.Map.empty)
           (implicit classes: String => Class){


  def method(name: String, desc: String): Option[Method] = {
    classFile.methods
             .find(m => m.name == name && m.desc == desc)
             .orElse(classFile.superName.flatMap(x => classes(x).method(name, desc)))
  }

  def name = classFile.name

  def isInstanceOf(desc: String)(implicit classes: String => Class): Boolean = {

    val res =
      classFile.name == desc ||
      classFile.interfaces.contains(desc) ||
      (classFile.superName.isDefined && classes(classFile.superName.get).isInstanceOf(desc))

    res
  }
}

object Object{
  def initMembers(cls: Class)(implicit classes: String => Class): List[(String, Any)] = {
    cls.classFile.fields.map{f =>
      f.name -> initField(f.desc)
    } ++ cls.classFile.superName.toSeq.flatMap(x => initMembers(classes(x)))
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


  def fromVirtual[T](x: Any): T = {
    def cloneArray[T](x: Array[T]): Array[T] = {
      val newArray = x.clone()
      for(i <- 0 until x.length){
        newArray(i) = fromVirtual[T](newArray(i))
      }
      newArray
    }
    x match{
      case null => null
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Long => x
      case x: Float => x
      case x: Double => x
      case x: Array[Any] => cloneArray(x)
      case x: Array[Boolean] => cloneArray(x)
      case x: Array[Byte] => cloneArray(x)
      case x: Array[Char] => cloneArray(x)
      case x: Array[Short] => cloneArray(x)
      case x: Array[Int] => cloneArray(x)
      case x: Array[Long] => cloneArray(x)
      case x: Array[Float] => cloneArray(x)
      case x: Array[Double] => cloneArray(x)
      case x: svm.Object if x.cls.name == "java/lang/String" =>
        new String(x.members("value").asInstanceOf[Array[Char]])
    }
  }.asInstanceOf[T]




  def toVirtual[T](x: Any)(implicit classes: String => Class): T = {
    def cloneArray  [T](x: Array[T])(implicit classes: String => Class): Array[T] = {
      val newArray = x.clone()
      for(i <- 0 until x.length){
        newArray(i) = toVirtual[T](newArray(i))
      }
      newArray
    }
    x match{
      case null => null
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Long => x
      case x: Float => x
      case x: Double => x
      case x: Array[Any] => cloneArray(x)
      case x: Array[Boolean] => cloneArray(x)
      case x: Array[Byte] => cloneArray(x)
      case x: Array[Char] => cloneArray(x)
      case x: Array[Short] => cloneArray(x)
      case x: Array[Int] => cloneArray(x)
      case x: Array[Long] => cloneArray(x)
      case x: Array[Float] => cloneArray(x)
      case x: Array[Double] => cloneArray(x)
      case x: String =>
        val r = new svm.Object(classes("java/lang/String"))
        r.members("value") = x.toCharArray
        r

    }
  }.asInstanceOf[T]
}

class Object(val cls: Class, val members: mutable.Map[String, Any] = mutable.Map.empty)(implicit classes: String => Class){
  for((k, v) <- Object.initMembers(cls)){
    members(k) = v
  }


  override def toString = {
    s"svm.Object(${cls.name})"
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