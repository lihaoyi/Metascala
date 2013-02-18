package svm.parsing

import java.nio.ByteBuffer
import akka.util.ByteString


class Lazy[T](n: Int, s: String, func: => T) extends Function0[T]{
  lazy val lFunc = {
    println("evaluating " + n + " -> " + s)
    val x = func
    println(x)
    x
  }
  def apply() = lFunc
}
class UnLazy[T](n: Int, s: String, func: T) extends Function0[T]{
  println(n + " " + s + " " + func)
  def apply() = func
}
object ConstantInfo{
  implicit class superByteBuffer(b: ByteBuffer){
    def fork(n: Int) = {
      //println("Forking " + n)
      val a = new Array[Byte](n)
      b.get(a)
      ByteBuffer.wrap(a)
    }
  }

  def partitionConstantPool(n: Int)(implicit input: ByteBuffer): Seq[() => Any] = {
    lazy val cp: Seq[() => Any] = (() => ZeroConstant) +: (for (i <- 1 to n) yield u1 match {
      case 7 =>  println(s"$i ClassRef"); val x = input.fork(2);  Seq(new Lazy(i, "ClassRef", ClassRef.read(cp, x)))
      case 9 =>  println(s"$i FieldRef"); val x = input.fork(4);  Seq(new Lazy(i, "FieldRef", FieldRef.read(cp, x)))
      case 10 => println(s"$i MethodRef"); val x = input.fork(4);  Seq(new Lazy(i, "MethodRef", MethodRef.read(cp, x)))
      case 11 => println(s"$i InterRef"); val x = input.fork(4);  Seq(new Lazy(i, "InterfaceMethodRef", InterfaceMethodRef.read(cp, x)))
      case 8 =>  println(s"$i StringIfo"); val x = input.fork(2);  Seq(new Lazy(i, "StringInfo", StringInfo.read(cp, x)))
      case 3 =>  val x = input.fork(4);  Seq(new UnLazy(i, "IntegerInfo", IntegerInfo.read(x)))
      case 4 =>  val x = input.fork(4);  Seq(new UnLazy(i, "FloatInfo", FloatInfo.read(x)))
      case 5 =>  val x = input.fork(8);  Seq(new UnLazy(i, "LongInfo", LongInfo.read(x)), () => ZeroConstant)
      case 6 =>  val x = input.fork(8);  Seq(new UnLazy(i, "DoubleInfo", DoubleInfo.read(x)), () => ZeroConstant)
      case 12 => println(s"$i NameType"); val x = input.fork(4);  Seq(new Lazy(i, "NameAndTypeInfo", NameAndType.read(cp, x)))
      case 1 =>  val x = input.fork(u2); Seq(new UnLazy(i, "Utf8Info", Utf8.read(x)))
    }).flatten

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

  object NameAndType{
    def read(implicit cp: Seq[() => Any], input: ByteBuffer) = {
      NameAndType(cp(u2)().asInstanceOf[String], cp(u2)().asInstanceOf[String])
    }
  }
  case class NameAndType(name: String, descriptor: String) {
    def value = (name, descriptor)
  }

  object Utf8{
    def read(implicit input: ByteBuffer) = {
      ByteString(input).decodeString("UTF-8")
    }
  }
}
