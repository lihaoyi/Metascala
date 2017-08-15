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
}

sealed trait WritableRef extends Ref{
  def update(i: Int): Unit
}

object Ref{


  /**
    * A manually set-able `Ref`. Note that you need to make sure whenever you
    * create one of these, it is hooked up in a way that the metascala.VM's
    * garbage collector can see it and update during a GC! The easiest way to
    * do this is through the `Allocator#{obj,arr,register}` functions
    */
  class UnsafeManual(var x: Int) extends WritableRef{
    def apply() = x
    def update(i: Int) = x = i
  }

  /**
    * Similar to `UnsafeManual`, but backed by an array+index instead of by
    * a local mutable cell
    */
  class UnsafeArr(val get: () => Int, val set: Int => Unit) extends WritableRef{
    def apply() = get()
    def update(i: Int) = set(i)
  }

  object Null extends WritableRef{
    def apply() = 0
    def update(i: Int) = assert(i == 0)
  }

  /**
    * A hard-coded, constant `Ref`. Note that this means the `Ref` will *not*
    * get moved during a GC; only use this in places where a `Ref` is needed
    * but the underlying value is a primitive type (which doesn't get moved)
    * or the value is a reference type but you are *very sure* the `Ref` will
    * be shorted lived and not survive across a GC
    */
  case class Raw(x: Int) extends Ref{
    def apply() = x
    def update(i: Int) = ???
  }
}
