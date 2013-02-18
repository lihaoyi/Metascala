package svm.parsing

import java.nio.ByteBuffer
import akka.util.ByteString



object ConstantInfo{
  implicit class superByteBuffer(b: ByteBuffer){
    def fork(n: Int) = {
      val a = new Array[Byte](n)
      b.get(a)
      ByteBuffer.wrap(a)
    }
  }

  // ByteBuffer => Seq[(
  def partitionConstantPool(n: Int)(implicit input: ByteBuffer): Seq[() => Any] = {
    lazy val cp: Seq[() => Any] = (for (i <- 1 to n) yield {
      val x = u1
      x match {
        case 7 =>  val x = input.fork(2);  Seq(() => ClassRef.read(cp, x))
        case 9 =>  val x = input.fork(4);  Seq(() => FieldRef.read(cp, x))
        case 10 => val x = input.fork(4);  Seq(() => MethodRef.read(cp, x))
        case 11 => val x = input.fork(4);  Seq(() => InterfaceMethodRef.read(cp, x))
        case 8 =>  val x = input.fork(2);  Seq(() => StringInfo.read(cp, x))
        case 3 =>  val x = input.fork(4);  Seq(() => IntegerInfo.read(cp, x))
        case 4 =>  val x = input.fork(4);  Seq(() => FloatInfo.read(cp, x))
        case 5 =>  val x = input.fork(8);  Seq(() => LongInfo.read(cp, x), () => ZeroConstant)
        case 6 =>  val x = input.fork(8);  Seq(() => DoubleInfo.read(cp, x), () => ZeroConstant)
        case 12 => val x = input.fork(4);  Seq(() => NameAndType.read(cp, x))
        case 1 =>  val x = input.fork(u2); Seq(() => Utf8.read(cp, x))
      }
    }).flatten

    (() => ZeroConstant) +: cp
  }

  object ZeroConstant{
    def value = ()
  }
  object ClassRef{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      ClassRef(cp(u2)().asInstanceOf[String])
    }
  }
  case class ClassRef(name: String){
    def value = ()
  }

  object FieldRef{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      FieldRef(cp(u2)().asInstanceOf[ClassRef], cp(u2)().asInstanceOf[NameAndType])
    }
  }
  case class FieldRef(classRef: ClassRef, nameAndType: NameAndType) {
    def value = ()
  }

  object MethodRef{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      MethodRef(cp(u2)().asInstanceOf[ClassRef], cp(u2)().asInstanceOf[NameAndType])
    }
  }
  case class MethodRef(classRef: ClassRef, nameAndType: NameAndType) {
    def value = ()
  }

  object InterfaceMethodRef{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      InterfaceMethodRef(cp(u2)().asInstanceOf[ClassRef], cp(u2)().asInstanceOf[NameAndType])
    }
  }
  case class InterfaceMethodRef(classRef: ClassRef, nameAndType: NameAndType) {
    def value = ()
  }

  object StringInfo{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      cp(u2)().asInstanceOf[String]
    }
  }

  object IntegerInfo{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      u4
    }
  }

  object FloatInfo{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      java.lang.Float.intBitsToFloat(u4)
    }
  }

  object LongInfo{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      (u4.toLong << 32) | u4 & 0xFFFFFFFFL
    }
  }

  object DoubleInfo{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      java.lang.Double.longBitsToDouble((u4.toLong << 32) | (u4 & 0xFFFFFFFFL))
    }
  }

  object NameAndType{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      NameAndType(cp(u2).asInstanceOf[String], cp(u2).asInstanceOf[String])
    }
  }
  case class NameAndType(name: String, descriptor: String) {
    def value = (name, descriptor)
  }

  object Utf8{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      ByteString(u(u2)).decodeString("UTF-8")
    }
  }
}
