package metascala.util

import scala.collection.mutable


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
object Constants{
  val objectHeaderSize = 2
  val arrayHeaderSize = 2

}