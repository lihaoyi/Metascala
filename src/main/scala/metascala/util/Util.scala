package metascala.util

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

class ArrayFiller(size: Int){
  val arr = new Array[Int](size)
  var index = 0
  def append(value: Int) = {
    arr(index) = value
    index += 1
  }
}
object Util{
  def shorten(name: String) = {
    val some :+ last = name.split("/").toSeq
    (some.map(_(0)) :+ last).mkString("/")
  }
  def shortedJava(name: String) = {
    val some :+ last = name.split('.').toSeq
    (some.map(_(0)) :+ last).mkString(".")
  }

  def blit(src: mutable.Seq[Int], srcIndex: Int, dest: mutable.Seq[Int], destIndex: Int, length: Int, flip: Boolean) = {
    if (!flip) dest(destIndex) = src(srcIndex)
    else src(srcIndex) = dest(destIndex)

    length match{
      case 1 =>
      case 2 =>
        if (!flip) dest(destIndex + 1) = src(srcIndex + 1)
        else src(srcIndex + 1) = dest(destIndex + 1)
    }

  }

  def reader(src: Seq[Int], index: Int) = {
    var i = index
    () => {
      i += 1
      src(i - 1)
    }
  }
  def writer(src: mutable.Seq[Int], index: Int) = {
    var i = index
    (x: Int) => {
      i += 1
      src(i - 1) = x
    }
  }

}

class WrappedVmException(wrapped: Throwable) extends Exception(wrapped)

/**
  * Something blew up within the code the VM is interpreting
  */
case class UncaughtVmException(wrapped: Throwable) extends WrappedVmException(wrapped)

/**
  * Something blew up within the VM's own code
  */
case class InternalVmException(wrapped: Throwable) extends WrappedVmException(wrapped)
