import collection.mutable

package object metascala {

  /**
   * Represents a number referencing the local variable pool.
   */
  type Sym = Int






  type Val = Int
  

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
