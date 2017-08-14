package metascala.util

/**
  * Holds a gettable/settable cell containing a single integer, typically a
  * memory address.
  *
  * Used instead of raw `Int`s because they play well with the garbage
  * collector: when a GC happens, and previously-allocated objects or arrays
  * are moved around, any `Ref`s you have access to will have their contents
  * automatically updated to the post-move memory address
  */
sealed trait Ref{
  def apply(): Int
  def update(i: Int): Unit
}
object Ref{
  class Arr(val get: () => Int, val set: Int => Unit) extends Ref{
    def apply() = get()
    def update(i: Int) = set(i)
  }

  object Null extends Ref{
    def apply() = 0
    def update(i: Int) = assert(i == 0)
  }
  class Manual(var x: Int) extends Ref{
    def apply() = x
    def update(i: Int) = x = i
  }
}
