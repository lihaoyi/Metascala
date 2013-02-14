package svm.model.immutable

import java.nio.ByteBuffer
import ConstantInfo.Utf8
import svm.model.immutable.AttributeInfo.InnerClasses.ClassData
import svm.model.immutable.AttributeInfo.LineNumberTable.LineNumberData
import svm.model.immutable.AttributeInfo.LocalVariableTable.LocalVariableData

object AttributeInfo{
  def read(implicit input: ByteBuffer, constant_pool: Seq[ConstantInfo]): AttributeInfo = {
    val attribute_name = constant_pool(u2)
    val attribute_length = u4
    attribute_name match {
      case Utf8("ConstantValue") => ConstantValue.read
      case Utf8("Code") => Code.read
      case Utf8("Exceptions") => Exceptions.read
      case Utf8("InnerClasses") => InnerClasses.read
      case Utf8("Synthetic") => Synthetic
      case Utf8("SourceFile") => SourceFile.read
      case Utf8("LineNumberTable") => LineNumberTable.read
      case Utf8("LocalVariableTable") => LocalVariableTable.read
      case Utf8("Deprecated") => Deprecated
    }
  }

  object ConstantValue{
    def read(implicit input: ByteBuffer) = {
      ConstantValue(u2)
    }
  }
  case class ConstantValue(constantvalue_index: u2)
                           extends AttributeInfo


  object Code{
    def read(implicit input: ByteBuffer, constant_pool: Seq[ConstantInfo]) = {
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
  case class Code(max_stack: u2,
                  max_locals: u2,
                  code: Seq[Byte],
                  exception_table: Seq[Code.ExceptionData],
                  attribute_info: Seq[AttributeInfo])
                  extends AttributeInfo

  object Exceptions{
    def read(implicit input: ByteBuffer) = {
      Exceptions(u2 ** u2)
    }
  }
  case class Exceptions(exception_index_table: Seq[u2]) extends AttributeInfo

  object InnerClasses{
    def read(implicit input: ByteBuffer) = {
      InnerClasses(
        u2 ** ClassData.read
      )
    }
    object ClassData{
      def read(implicit input: ByteBuffer) = {
        ClassData(
          u2,
          u2,
          u2,
          u2
        )
      }
    }
    case class ClassData(inner_class_info_index: u2,
                         outer_class_info_index: u2,
                         inner_name_index: u2,
                         inner_class_access_flags: u2)
                         extends AttributeInfo
  }
  case class InnerClasses(classes: Seq[ClassData]) extends AttributeInfo

  case object Synthetic extends AttributeInfo

  object SourceFile{
    def read(implicit input: ByteBuffer) = {
      SourceFile(u2)
    }
  }
  case class SourceFile(sourcefile_index: u2)
                       extends AttributeInfo

  object LineNumberTable{
    def read(implicit input: ByteBuffer) = {
      LineNumberTable(
        u2 ** LineNumberData.read
      )
    }
    object LineNumberData{
      def read(implicit input: ByteBuffer) = {
        LineNumberData(u2, u2)
      }
    }
    case class LineNumberData(start_pc: u2,
                              line_number: u2)
  }
  case class LineNumberTable(line_number_table: Seq[LineNumberData])
                             extends AttributeInfo

  object LocalVariableTable{
    def read(implicit input: ByteBuffer) = {
      LocalVariableTable(
        u2 ** LocalVariableData.read
      )
    }

    object LocalVariableData{
      def read(implicit input: ByteBuffer) = {
        LocalVariableData(
          u2,
          u2,
          u2,
          u2,
          u2
        )
      }
    }
    case class LocalVariableData(start_pc: u2,
                                 length: u2,
                                 name_index: u2,
                                 descriptor_index: u2,
                                 index: u2)

  }
  case class LocalVariableTable(local_variable_table: Seq[LocalVariableData])
                                extends AttributeInfo


  case object Deprecated extends AttributeInfo
}

trait AttributeInfo
