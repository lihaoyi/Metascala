package svm.parsing

import java.nio.ByteBuffer
import ConstantInfo._
import svm.parsing.Attribute.InnerClasses.ClassData
import svm.parsing.Attribute.LineNumberTable.LineNumberData
import svm.parsing.Attribute.LocalVariableTable.LocalVariableData

object Attribute{
  def read(implicit cp: Seq[Any], input: ByteBuffer): Attribute = {
    val index = u2
    val attribute_name = cp(index)
    val attribute_length = u4
    attribute_name match {
      case "ConstantValue" => ConstantValue.read
      case "Code" => Code.read
      case "Exceptions" => Exceptions.read
      case "InnerClasses" => InnerClasses.read
      case "Synthetic" => Synthetic
      case "Signature" => Signature.read
      case "SourceFile" => SourceFile.read
      case "LineNumberTable" => LineNumberTable.read
      case "LocalVariableTable" => LocalVariableTable.read
      case "Deprecated" => Deprecated
      case "StackMapTable" =>
        input.get(new Array[Byte](attribute_length))
        new Attribute {}
    }
  }

  object ConstantValue{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      ConstantValue(cp(u2))
    }
  }
  case class ConstantValue(constantValue: Any)
                           extends Attribute

  object Code{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      Code(
        u2,
        u2,
        u(u4),
        u2 ** ExceptionData.read,
        u2 ** Attribute.read
      )
    }
    object ExceptionData{
      def read(implicit cp: Seq[Any], input: ByteBuffer) = {
        ExceptionData(u2, u2, u2, cp(u2).asInstanceOf[ClassRef])
      }
    }
    case class ExceptionData(start_pc: u2, end_pc: u2, handler_pc: u2, catch_type: ClassRef)
  }
  case class Code(max_stack: u2,
                  max_locals: u2,
                  code: Seq[Byte],
                  exception_table: Seq[Code.ExceptionData],
                  attribute_info: Seq[Attribute])
                  extends Attribute

  object Exceptions{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      Exceptions(u2 ** cp(u2).asInstanceOf[ClassRef])
    }
  }
  case class Exceptions(exceptionTable: Seq[ClassRef]) extends Attribute

  object InnerClasses{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      InnerClasses(
        u2 ** ClassData.read
      )
    }
    object ClassData{
      def read(implicit cp: Seq[Any], input: ByteBuffer) = {
        ClassData(
          cp(u2).asInstanceOf[ClassRef],
          cp(u2).asInstanceOf[ClassRef],
          cp(u2).asInstanceOf[String],
          u2
        )
      }
    }
    case class ClassData(innerClass: ClassRef,
                         outerClass: ClassRef,
                         innerName: String,
                         inner_class_access_flags: u2)
                         extends Attribute
  }
  case class InnerClasses(classes: Seq[ClassData]) extends Attribute

  case object Synthetic extends Attribute

  object Signature{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      Signature(cp(u2).asInstanceOf[String])
    }
  }
  case class Signature(signature: String)
    extends Attribute

  object SourceFile{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      SourceFile(cp(u2).asInstanceOf[String])
    }
  }
  case class SourceFile(sourceFile: String)
                       extends Attribute

  object LineNumberTable{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      LineNumberTable(
        u2 ** LineNumberData.read
      )
    }
    object LineNumberData{
      def read(implicit cp: Seq[Any], input: ByteBuffer) = {
        LineNumberData(u2, u2)
      }
    }
    case class LineNumberData(start_pc: u2,
                              line_number: u2)
  }
  case class LineNumberTable(line_number_table: Seq[LineNumberData])
                             extends Attribute

  object LocalVariableTable{
    def read(implicit cp: Seq[Any], input: ByteBuffer) = {
      LocalVariableTable(
        u2 ** LocalVariableData.read
      )
    }

    object LocalVariableData{
      def read(implicit cp: Seq[Any], input: ByteBuffer) = {
        LocalVariableData(
          u2,
          u2,
          cp(u2).asInstanceOf[String],
          cp(u2).asInstanceOf[String],
          u2
        )
      }
    }
    case class LocalVariableData(start_pc: u2,
                                 length: u2,
                                 name: String,
                                 descriptor: String,
                                 index: u2)

  }
  case class LocalVariableTable(local_variable_table: Seq[LocalVariableData])
                                extends Attribute


  case object Deprecated extends Attribute
}

trait Attribute
