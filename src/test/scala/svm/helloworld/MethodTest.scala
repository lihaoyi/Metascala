package svm.helloworld

import org.scalatest.FreeSpec

import svm.{Gen, Util}
import svm.Gen._

class MethodTest extends FreeSpec with Util{


  implicit val intAll10 = 10 ** Gen.intAll

  "static" - {
    val tester = new Tester("svm.helloworld.methods.Statics")
    "helloWorld" in chk(tester.run("helloWorld", _: Int))
    "helloWorld2" in chk(tester.run("helloWorld2", _: Int, _: Int))
    "tailFactorial" in chk(tester.run("tailFactorial", _: Int))(Seq(2, 5, 10, 20, 50))
    "fibonacci" in chk(tester.run("fibonacci", _: Int))(Seq(2, 5, 10))

  }
  "natives" - {
    val tester = new Tester("svm.helloworld.methods.Natives")
    "intBitsToFloat" in chk(tester.run("intBitsToFloat", _: Int))
    "currentTimeMillis" in tester.run("currentTimeMillis")
  }
  "objects" - {
    val tester = new Tester("svm.helloworld.methods.Objects")
    "dumbobjects" in tester.run("helloWorld", 5)
    "inheritance" in tester.run("inheritance", 5)
    "points" in chk(tester.run("points", _: Int))
    "points2" in chk(tester.run("points", _: Int))
  }
}

