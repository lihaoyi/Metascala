import metascala.rt.{Arr, Obj}
import collection.mutable
import scala.reflect.ClassTag

package object metascala {

  /**
   * Represents a number referencing the local variable pool.
   */
  type Sym = Int




  def isObj(i: Int) = i < 0
  def isArr(i: Int) = i > 0
  implicit class pimpedVal(val v: Val) extends AnyVal{
    def isObj(implicit vm: VMInterface) = metascala.isObj(vm.heap(v))
    def isArr(implicit vm: VMInterface) = metascala.isArr(vm.heap(v))
    def obj(implicit vm: VMInterface) = {

      //assert(isObj, v + " " + vm.heap.memory.slice(v / 10 * 10, v / 10 * 10 + 10).toList)
      new Obj(v)
    }
    def arr(implicit vm: VMInterface) = {
//      assert(isArr, v)
      new Arr(v)
    }
  }



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
