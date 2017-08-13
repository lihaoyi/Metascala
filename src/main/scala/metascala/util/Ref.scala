package metascala.util

trait Ref{
  def apply(): Int
  def update(i: Int): Unit
}
object Ref{
  class ArrRef(val get: () => Int, val set: Int => Unit) extends Ref{
    def apply() = get()
    def update(i: Int) = set(i)
  }

  implicit class ManualRef(var x: Int) extends Ref{
    def apply() = x
    def update(i: Int) = x = i
  }
}
