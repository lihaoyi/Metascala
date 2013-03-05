package svm.full

import svm.{Gen, Util}
import org.scalatest.FreeSpec

class SVMTest extends FreeSpec with Util{
  /*"omg" in {
    val tester = new Tester("svm.full.SVM")
    println("Hello World")
    tester.run("initSVM")
  }*/
  "rhino" in {
    val tester = new Tester("svm.full.SVM")

    tester.run("rhino")
  }
}
