package svm

import model.ClassFile

class Class(val classFile: ClassFile, _statics: collection.mutable.Map[String, Any] = collection.mutable.Map.empty){
  object statics{
    def apply(name: String) = _statics(name)
    def update(name: String, value: Any) = {
      val field = classFile.fields.find(_.name == name).get

      _statics(name) = value
    }
  }
  def name = classFile.this_class.name
}

class Object(val cls: Class, val members: collection.mutable.Map[String, Any] = collection.mutable.Map.empty)

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