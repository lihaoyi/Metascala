package svm.model

import java.nio.ByteBuffer

package object immutable {
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
}
