package svm.full

import svm.{Gen, Util}
import org.scalatest.FreeSpec
import svm.Util.SingleClassVM

object Moo{
  def run() = {
    val x = new svm.Util.SingleClassVM("svm.features.controlflow.Loops", s => ())
    x.run("sqrtFinder", 5.0)
  }
}

class SVMTest extends FreeSpec with Util{
  "omg" in {
    val tester = new Tester("svm.full.Moo")
    tester.run("run")
  }
  /*"rhino" in {
    val tester = new Tester("svm.full.SVM")
    tester.run("rhino")
  }*/

}

