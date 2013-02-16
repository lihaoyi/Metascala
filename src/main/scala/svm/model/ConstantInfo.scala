package svm.model

import java.nio.ByteBuffer
import akka.util.ByteString

class ConstantInfo(tag: Byte)
object ConstantInfo{
  def item(implicit input: ByteBuffer) = {
    val x = u1
    x match{
      case 7 => ClassRef.read
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
  object ClassRef{
    def read(implicit input: ByteBuffer) = ClassRef(u2)
  }
  case class ClassRef(name_index: u2) extends ConstantInfo(7)

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
