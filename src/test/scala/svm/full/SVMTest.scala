package svm.full

import svm.{Gen, Util}
import org.scalatest.FreeSpec
import svm.Util.SingleClassVM

object Moo{
  def run() = {
    val x = svm.Util.singleClassVm("svm.math.HelloMath")
    x.run("imain")
  }
}
class SVMTest extends FreeSpec with Util{
  "omg" in {
    val tester = new Tester("svm.full.Moo")

    tester.run("run")
  }
  "thing" in {
    println(System.getProperty("java.lang.Integer.IntegerCache.high"))
  }
  /*"rhino" in {
    val tester = new Tester("svm.full.SVM")
    tester.run("rhino")
  }*/

}

