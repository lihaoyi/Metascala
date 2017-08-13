package metascala

import scala.collection.mutable


/**
  * Convenience methods to provide a safe way of converting null Lists, Arrays
  * or Objects into empty Lists, Arrays or None
  */
object NullSafe{
  import scala.collection.JavaConverters._
  def apply[T](list: java.util.List[T]): Seq[T] = {
    Option(list).map(_.iterator.asScala).toVector.flatten
  }

  def apply[T](list: Array[T]): Seq[T] = Option(list).toVector.flatten

  def apply[T](a: T): Option[T] = Option(a)
}

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
object Util{
  def shorten(name: String) = {
    val some :+ last = name.split("/").toSeq
    (some.map(_(0)) :+ last).mkString("/")
  }

  def blit(src: Seq[Int], srcIndex: Int, dest: mutable.Seq[Int], destIndex: Int, length: Int) = {
    var i = 0
    while(i < length){
      dest(destIndex + i) = src(srcIndex + i)
      i+= 1
    }
  }
}

class WrappedVmException(wrapped: Throwable) extends Exception(wrapped)
case class UncaughtVmException(wrapped: Throwable) extends WrappedVmException(wrapped)
case class InternalVmException(wrapped: Throwable) extends WrappedVmException(wrapped)

/**
  * A generic cache, which provides pre-processing of keys and post processing of values.
  */
trait Cache[In, Out] extends (In => Out){
  val cache = mutable.Map.empty[Any, Out]
  def pre(x: In): Any = x
  def calc(x: In): Out
  def post(y: Out): Unit = ()
  def apply(x: In) = {
    val newX = pre(x)
    cache.get(newX) match{
      case Some(y) => y
      case None =>
        val newY = calc(x)
        cache(newX) = newY
        post(newY)
        newY
    }
  }
}