package metascala.full

import metascala.Util
import org.scalatest.FreeSpec
object GCTestsBasic{
  def helloObj(n: Int) = {
    var i = 0
    while(i < n){
      val p = new Object()
      i += 1
    }
    i
  }
  def helloArr(n: Int) = {
    var i = 0
    val p = new Array[Int](2)
    p(0) = 5
    p(1) = 6
    val p2 = Array(p)

    while(i < n){
      val p = new Array[Int](3)
      p(0) = 9
      p(1) = 8
      p(2) = 7
      i += 1
    }
    p2(0)(1)
  }
}
object GCTestsTwo{

}
class GCTests extends FreeSpec with Util{

  "helloObj" in {
    val tester = new Tester("metascala.full.GCTestsBasic", memorySize = 20)
    tester.run("helloObj", 20)
  }

  "helloArr" in {
    val tester = new Tester("metascala.full.GCTestsBasic", memorySize = 20)
    tester.run("helloArr", 10)
  }

}
