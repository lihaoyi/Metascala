package svm.helloworld

import org.scalatest.FreeSpec

import svm.Util
import svm.Gen._

class MethodTest extends FreeSpec with Util{


  "static" - {
    val tester = new Tester("svm.helloworld.methods.Statics")
    "helloWorld" in check(tester.run("helloWorld", _: Int), 5)
    "helloWorld2" in check(tester.run("helloWorld2", _: Int, _: Int), 5)
    "tailFactorial" in check(tester.run("tailFactorial", _: Int), 5)(Seq(2, 5, 10, 20, 50))
    "fibonacci" in check(tester.run("fibonacci", _: Int), 3)(Seq(2, 5, 10))

  }

}

