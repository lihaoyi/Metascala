package svm.parsing

import java.nio.ByteBuffer
import akka.util.ByteString



object ConstantInfo{
  def eagerVal[T](func: => T) = {
    val lFunc = func
    () => lFunc
  }
  def lazyVal[T](func: => T) = {
    lazy val lFunc = func
    () => lFunc
  }

  implicit class superByteBuffer(b: ByteBuffer){
    def fork(n: Int) = {
      //println("Forking " + n)
      val a = new Array[Byte](n)
      b.get(a)
      ByteBuffer.wrap(a)
    }
  }

  def partitionConstantPool(n: Int)(implicit input: ByteBuffer): Seq[() => Any] = {
    val lazyZero = () => ZeroConstant
    lazy val cp: Seq[() => Any] = {
      var total = Seq(Seq[() => Any](lazyZero))
      var count = 1

      while(count <= n){

        val next = u1 match {
          case 7  => val x = input.fork(2);  Seq(lazyVal(ClassRef.read(cp, x)))
          case 9  => val x = input.fork(4);  Seq(lazyVal(FieldRef.read(cp, x)))
          case 10 => val x = input.fork(4);  Seq(lazyVal(MethodRef.read(cp, x)))
          case 11 => val x = input.fork(4);  Seq(lazyVal(InterfaceMethodRef.read(cp, x)))
          case 8  => val x = input.fork(2);  Seq(lazyVal(StringInfo.read(cp, x)))
          case 3  => val x = input.fork(4);  Seq(eagerVal(IntegerInfo.read(x)))
          case 4  => val x = input.fork(4);  Seq(eagerVal(FloatInfo.read(x)))
          case 5  => val x = input.fork(8);  Seq(eagerVal(LongInfo.read(x)), lazyZero)
          case 6  => val x = input.fork(8);  Seq(eagerVal(DoubleInfo.read(x)), lazyZero)
          case 12 => val x = input.fork(4);  Seq(lazyVal(NameAndTypeInfo.read(cp, x)))
          case 1  => val x = input.fork(u2); Seq(eagerVal(Utf8Info.read(x)))
        }
        total = total :+ next
        count += next.length
      }
      total
    }.flatten

    cp
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
      FieldRef(cp(u2)().asInstanceOf[ClassRef], cp(u2)().asInstanceOf[NameAndTypeInfo])
    }
  }
  case class FieldRef(classRef: ClassRef, nameAndType: NameAndTypeInfo) {
    def value = ()
  }

  object MethodRef{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {

      MethodRef(cp(u2)().asInstanceOf[ClassRef], cp(u2)().asInstanceOf[NameAndTypeInfo])
    }
  }
  case class MethodRef(classRef: ClassRef, nameAndType: NameAndTypeInfo) {
    def value = ()
  }

  object InterfaceMethodRef{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      InterfaceMethodRef(cp(u2)().asInstanceOf[ClassRef], cp(u2)().asInstanceOf[NameAndTypeInfo])
    }
  }
  case class InterfaceMethodRef(classRef: ClassRef, nameAndType: NameAndTypeInfo) {
    def value = ()
  }

  object StringInfo{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      cp(u2)().asInstanceOf[String]
    }
  }

  object IntegerInfo{
    def read(implicit input: ByteBuffer) = {
      u4
    }
  }

  object FloatInfo{
    def read(implicit input: ByteBuffer) = {
      java.lang.Float.intBitsToFloat(u4)
    }
  }

  object LongInfo{
    def read(implicit input: ByteBuffer) = {
      (u4.toLong << 32) | u4 & 0xFFFFFFFFL
    }
  }

  object DoubleInfo{
    def read(implicit input: ByteBuffer) = {
      java.lang.Double.longBitsToDouble((u4.toLong << 32) | (u4 & 0xFFFFFFFFL))
    }
  }

  object NameAndTypeInfo{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      NameAndTypeInfo(cp(u2)().asInstanceOf[String], cp(u2)().asInstanceOf[String])
    }
  }
  case class NameAndTypeInfo(name: String, descriptor: String) {
    def value = (name, descriptor)
  }

  object Utf8Info{
    def read(implicit input: ByteBuffer) = {
      ByteString(input).decodeString("UTF-8")
    }
  }
}
