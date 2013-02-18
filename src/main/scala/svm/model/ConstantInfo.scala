package svm.model

import java.nio.ByteBuffer
import akka.util.ByteString

abstract class ConstantInfo(tag: Byte){
  def value(implicit constant_pool: Seq[ConstantInfo]): Any
}
object ConstantInfo{
  def item(implicit input: ByteBuffer) = {
    val x = u1
    val res = x match{
      case 7 => Seq(ClassRef.read)
      case 9 => Seq(FieldRef.read)
      case 10 => Seq(MethodRef.read)
      case 11 => Seq(InterfaceMethodRef.read)
      case 8 => Seq(StringInfo.read)
      case 3 => Seq(IntegerInfo.read)
      case 4 => Seq(FloatInfo.read)
      case 5 => Seq(LongInfo.read, DummyZeroConstant)
      case 6 => Seq(DoubleInfo.read, DummyZeroConstant)
      case 12 => Seq(NameAndType.read)
      case 1 => Seq(Utf8.read)
    }
    //println(res)
    res
  }
  object DummyZeroConstant extends ConstantInfo(-1){
    def value(implicit constant_pool: Seq[ConstantInfo]) = ()
  }
  object ClassRef{ def read(implicit input: ByteBuffer) = ClassRef(u2) }
  case class ClassRef(name_index: u2) extends ConstantInfo(7){
    def value(implicit constant_pool: Seq[ConstantInfo]) = ()
  }

  object FieldRef{ def read(implicit input: ByteBuffer) = FieldRef(u2, u2) }
  case class FieldRef(class_index: u2, name_and_type_index: u2) extends ConstantInfo(9){
    def value(implicit constant_pool: Seq[ConstantInfo]) = ()
  }

  object MethodRef{ def read(implicit input: ByteBuffer) = MethodRef(u2, u2) }
  case class MethodRef(class_index: u2, name_and_type_index: u2) extends ConstantInfo(10){
    def value(implicit constant_pool: Seq[ConstantInfo]) = ()
  }

  object InterfaceMethodRef{ def read(implicit input: ByteBuffer) = InterfaceMethodRef(u2, u2) }
  case class InterfaceMethodRef(class_index: u2, name_and_type_index: u2) extends ConstantInfo(11){
    def value(implicit constant_pool: Seq[ConstantInfo]) = ()
  }

  object StringInfo{ def read(implicit input: ByteBuffer) = StringInfo(u2) }
  case class StringInfo(string_index: u2) extends ConstantInfo(8){
    def value(implicit constant_pool: Seq[ConstantInfo]) = constant_pool(string_index).value
  }

  object IntegerInfo{ def read(implicit input: ByteBuffer) = IntegerInfo(u4) }
  case class IntegerInfo(bytes: u4) extends ConstantInfo(3){
    def value(implicit constant_pool: Seq[ConstantInfo]) = bytes
  }

  object FloatInfo{ def read(implicit input: ByteBuffer) = FloatInfo(u4) }
  case class FloatInfo(bytes: u4) extends ConstantInfo(4){
    def value(implicit constant_pool: Seq[ConstantInfo]) = java.lang.Float.intBitsToFloat(bytes)
  }

  object LongInfo{ def read(implicit input: ByteBuffer) = LongInfo(u4, u4) }
  case class LongInfo(high_bytes: u4, low_bytes: u4) extends ConstantInfo(5){
    def value(implicit constant_pool: Seq[ConstantInfo]) = (high_bytes.toLong << 32) | low_bytes & 0xFFFFFFFFL
  }

  object DoubleInfo{ def read(implicit input: ByteBuffer) = DoubleInfo(u4, u4) }
  case class DoubleInfo(high_bytes: u4, low_bytes: u4) extends ConstantInfo(6){
    def value(implicit constant_pool: Seq[ConstantInfo]) = {
      val longBits = (high_bytes.toLong << 32) | (low_bytes & 0xFFFFFFFFL)
      java.lang.Double.longBitsToDouble(longBits)
    }
  }

  object NameAndType{ def read(implicit input: ByteBuffer) = NameAndType(u2, u2) }
  case class NameAndType(name_index: u2, descriptor_index: u2) extends ConstantInfo(12){
    def value(implicit constant_pool: Seq[ConstantInfo]) = ()
  }

  object Utf8{ def read(implicit input: ByteBuffer) = Utf8(ByteString(u(u2)).decodeString("UTF-8")) }
  case class Utf8(body: java.lang.String) extends ConstantInfo(1){
    def value(implicit constant_pool: Seq[ConstantInfo]) = body
  }
}
