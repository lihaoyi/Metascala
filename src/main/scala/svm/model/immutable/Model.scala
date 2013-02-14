package svm.model.immutable

import io.Source
import java.io.FileInputStream
import java.nio.ByteBuffer
import akka.util.ByteString
import svm.model._
import immutable.ConstantInfo.DummyZeroConstant


object ClassFile{
  def read(implicit input: ByteBuffer) = {
    val magic = u4
    val minor_version = u2
    val major_version = u2
    implicit val constant_pool = DummyZeroConstant +: (u2-1) ** ConstantInfo.item(input)

    ClassFile(
      magic,
      minor_version,
      major_version,
      constant_pool,
      u2,
      u2,
      u2,
      u2 ** u2,
      u2 ** FieldInfo.read,
      u2 ** MethodInfo.read,
      u2 ** AttributeInfo.read
    )

  }
}
case class ClassFile(magic: u4,
                     minor_version: u2,
                     major_version: u2,
                     constant_pool: Seq[ConstantInfo],
                     access_flags: u2,
                     this_class: u2,
                     super_class: u2,
                     interfaces: Seq[u2],
                     fields: Seq[FieldInfo],
                     methods: Seq[MethodInfo],
                     attributes: Seq[AttributeInfo])
class ConstantInfo(tag: Byte)
object ConstantInfo{
  def item(implicit input: ByteBuffer) = {
    val x = u1
    x match{
      case 7 => Class.read
      case 9 => FieldRef.read
      case 10 => MethodRef.read
      case 11 => InterfaceMethodRef.read
      case 8 => StringInfo.read
      case 3 => IntegerInfo.read
      case 4 => FloatInfo.read
      case 5 => LongInfo.read
      case 6 => DoubleInfo.read
      case 12 => NameAndType.read
      case 1 => Utf8.read
    }
  }
  object DummyZeroConstant extends ConstantInfo(-1)
  object Class{
    def read(implicit input: ByteBuffer) = Class(u2)
  }
  case class Class(name_index: u2) extends ConstantInfo(7)

  object FieldRef{
    def read(implicit input: ByteBuffer) = FieldRef(u2, u2)
  }
  case class FieldRef(class_index: u2, name_and_type_index: u2) extends ConstantInfo(9)

  object MethodRef{
    def read(implicit input: ByteBuffer) = MethodRef(u2, u2)
  }
  case class MethodRef(class_index: u2, name_and_type_index: u2) extends ConstantInfo(10)

  object InterfaceMethodRef{
    def read(implicit input: ByteBuffer) = InterfaceMethodRef(u2, u2)
  }
  case class InterfaceMethodRef(class_index: u2, name_and_type_index: u2) extends ConstantInfo(11)

  object StringInfo{
    def read(implicit input: ByteBuffer) = StringInfo(u2)
  }
  case class StringInfo(string_index: u2) extends ConstantInfo(8)

  object IntegerInfo{
    def read(implicit input: ByteBuffer) = IntegerInfo(u4)
  }
  case class IntegerInfo(bytes: u4) extends ConstantInfo(3)

  object FloatInfo{
    def read(implicit input: ByteBuffer) = FloatInfo(u4)
  }
  case class FloatInfo(bytes: u4) extends ConstantInfo(4)

  object LongInfo{
    def read(implicit input: ByteBuffer) = LongInfo(u4, u4)
  }
  case class LongInfo(high_bytes: u4, low_bytes: u4) extends ConstantInfo(5)

  object DoubleInfo{
    def read(implicit input: ByteBuffer) = DoubleInfo(u4, u4)
  }
  case class DoubleInfo(high_bytes: u4, low_bytes: u4) extends ConstantInfo(6)

  object NameAndType{
    def read(implicit input: ByteBuffer) = NameAndType(u2, u2)
  }
  case class NameAndType(name_index: u2, descriptor_index: u2) extends ConstantInfo(12)

  object Utf8{
    def read(implicit input: ByteBuffer) = {
      val length = u2
      Utf8(ByteString(u(length)).decodeString("UTF-8"))
    }
  }
  case class Utf8(body: java.lang.String) extends ConstantInfo(1)
}

object FieldInfo{
  def read(implicit input: ByteBuffer, constant_pool: Seq[ConstantInfo]) = {
    FieldInfo(
      u2,
      u2,
      u2,
      u2**AttributeInfo.read
    )
  }
}
case class FieldInfo(access_flags: u2,
                     name_index: u2,
                     descriptor_index: u2,
                     attributes_info: Seq[AttributeInfo])

object MethodInfo{
  def read(implicit input: ByteBuffer, constant_pool: Seq[ConstantInfo]) = {
    MethodInfo(
      u2,
      u2,
      u2,
      u2 ** AttributeInfo.read
    )
  }
}
case class MethodInfo(access_flags: u2,
                      name_index: u2,
                      descriptor_index: u2,
                      attributes: Seq[AttributeInfo])

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
