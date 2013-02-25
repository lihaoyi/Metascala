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
  "natives" - {
    val tester = new Tester("svm.helloworld.methods.Natives")
    "intBitsToFloat" in check(tester.run("intBitsToFloat", _: Int), 10)
    "currentTimeMillis" in tester.run("currentTimeMillis")
  }
  "objects" - {
    val tester = new Tester("svm.helloworld.methods.Objects")
    "dumbobjects" in tester.run("helloWorld", 5)
    "inheritance" in tester.run("inheritance", 5)
    "points" in check(tester.run("points", _: Int), 5)
    "points2" in check(tester.run("points", _: Int), 5)
  }
}

