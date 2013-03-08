package svm.full

import svm.{Gen, Util}
import org.scalatest.FreeSpec
import svm.Util.SingleClassVM

object Moo{
  def run() = {
    val x = new svm.Util.SingleClassVM("svm.helloworld.controlflow.Loops")
    x.run("sqrtFinder", 5.0)
  }
  def main(args: Array[String]){
    println(run())
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

