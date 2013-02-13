package svm


import io.Source
import java.io.FileInputStream
import java.nio.ByteBuffer
import akka.util.ByteString
import svm.ClassLoader.ConstantInfo.DummyZeroConstant

object ClassLoader extends {
  def u1(implicit input: ByteBuffer) = input.get
  type u1 = Byte
  def u2(implicit input: ByteBuffer) = input.getShort
  type u2 = Short
  def u4(implicit input: ByteBuffer) = input.getInt
  type u4 = Int
  def u(n: Int)(implicit input: ByteBuffer) = {
    val x = new Array[Byte](n)
    input.get(x)
    x
  }

  implicit def createArray(n: Short) = new createArray(n: Int)
  implicit class createArray[A](n: Int){
    def **[A](f: => A) = (0 until n).map{_ => f}
  }
  object ClassFile{
    def read(implicit input: ByteBuffer) = {
      ClassFile(
        u4,
        u2,
        u2,
        DummyZeroConstant +: (u2-1) ** ConstantInfo.item(input),
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
    def read(implicit input: ByteBuffer) = {
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



  object AttributeInfo{
    def read(implicit input: ByteBuffer)  = {
      AttributeInfo(
        u2,
        u(u4)
      )
    }

    object ConstantValue{
      def read(implicit input: ByteBuffer) = {
        ConstantValue(u2, u4, u2)
      }
    }
    case class ConstantValue(attribute_name_index: u2,
                                      attribute_length: u4,
                                      constantvalue_index: u2)

    object Code{
      def read(implicit input: ByteBuffer) = {
        Code(
          u2,
          u2,
          u(u4),
          u2 ** ExceptionData.read,
          u2 ** AttributeInfo.read
        )
      }
      object ExceptionData{
        def read(implicit input: ByteBuffer) = ExceptionData(u2, u2, u2, u2)
      }
      case class ExceptionData(start_pc: u2, end_pc: u2, handler_pc: u2, catch_type: u2)
    }
    case class Code(
      max_stack: u2,
      max_locals: u2,
      code: Seq[Byte],
      exception_table: Seq[Code.ExceptionData],
      attribute_info: Seq[AttributeInfo]
    )

    object Exceptions{
      def read(implicit input: ByteBuffer) = {
        Exceptions(u2 ** u2)
      }
    }
    case class Exceptions(exception_index_table: Seq[u2])
  }

  case class AttributeInfo(attribute_name_index: u2,
                           info: Array[u1])


  object MethodInfo{
    def read(implicit input: ByteBuffer) = {
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
}







