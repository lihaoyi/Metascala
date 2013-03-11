package sm.full

import sm.{Gen, Util}
import org.scalatest.FreeSpec
import sm.Util.SingleClassVM

object Moo{
  def run() = {
    val x = new sm.Util.SingleClassVM("sm.features.controlflow.Loops", s => ())
    x.run("sqrtFinder", 5.0)
  }
}

class SVMTest extends FreeSpec with Util{
  "omg" in {
    val tester = new Tester("sm.full.Moo")
    tester.run("run")
  }
  /*"rhino" in {
    val tester = new Tester("sm.full.SVM")
    tester.run("rhino")
  }*/

}

