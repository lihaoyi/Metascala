package sm
import collection.convert.wrapAsScala._

/**
 * This vrt contains the code involved in reading the .class files
 * (using ASM) and generating an immutable representation of all the data
 * structures encoded in the class file.
 *
 * There should be no mutable state in this vrt; any operations on mutable
 * state defined in this vrt should have their mutable variables injected
 * in as method parameters.
 */
package object imm {
  private[imm] implicit class nullSafeList[T](val list: java.util.List[T]) extends AnyVal{
    def safeList: List[T] = {
      Option(list).toList.flatten
    }
  }
  private[imm] implicit class nullSafeArray[T](val list: Array[T]) extends AnyVal{
    def safeList: List[T] = {
      Option(list).toList.flatten
    }
  }
  private[imm] implicit class nullSafeValue[T](val a: T) extends AnyVal{
    def safeOpt: Option[T] = Option(a)
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
}
