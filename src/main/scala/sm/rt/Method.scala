package sm
package rt



/**
 * A reference to a method with a specific signature
 */
trait Method{
  def name: String
  def desc: imm.Desc
}
object Method{

  /**
   * A reference to a native method at a particular index in the
   * native method table
   */
  case class Native(index: Int)(implicit vm: VM) extends Method{
    lazy val name = vm.natives.trappedIndex(index)._1._1.reverse.takeWhile(_ != '/').reverse
    lazy val desc = vm.natives.trappedIndex(index)._1._2
  }

  /**
   * A reference to a method belonging to a class
   */
  case class Cls(clsIndex: Int, methodIndex: Int)(implicit vm: VM) extends Method{
    assert(clsIndex >= 0, "clsIndex can't be negative")
    assert(methodIndex >= 0, "index can't be negative")
    lazy val name = vm.ClsTable.clsIndex(clsIndex).clsData.methods(methodIndex).name

    lazy val desc = vm.ClsTable.clsIndex(clsIndex).clsData.methods(methodIndex).desc
  }
}
