package svm.model

import java.nio.ByteBuffer
import ConstantInfo.DummyZeroConstant


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