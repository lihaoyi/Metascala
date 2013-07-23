package metascala.full

import metascala.Util
import org.scalatest.FreeSpec
object GCTests{
  def allocDeAlloc(n: Int) = {
    var i = 0
    while(i < n){
      val p = new Object()
      i += 1
    }
    i
  }
}
class GCTests extends FreeSpec with Util{
  val tester = new Tester("metascala.full.GCTests")
  "allocDeAlloc" in {
    tester.run("allocDeAlloc", 10)
  }
}
