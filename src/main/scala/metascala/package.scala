import metascala.rt.{Arr, Obj}
import collection.mutable
import scala.reflect.ClassTag

package object metascala {

  /**
   * Represents a number referencing the local variable pool.
   */
  type Sym = Int






  type Val = Int



  implicit def stringToClass(s: String)(implicit vm: VMInterface) = vm.ClsTable(imm.Type.Cls(s))
  implicit def stringToClsType(s: String) = imm.Type.Cls.apply(s)

  def reader(src: Seq[Val], index: Int) = {
    var i = index
    () => {
      i += 1
      src(i - 1)
    }
  }
  def writer(src: mutable.Seq[Val], index: Int) = {
    var i = index
    (x: Int) => {
      i += 1
      src(i - 1) = x
    }
  }



}
