package metascala.features

import org.scalatest.FreeSpec

import metascala.{Gen, Util}
import metascala.Gen._

class MethodTest extends FreeSpec with Util{


  implicit val intAll10 = 10 ** Gen.intAll

  "static" - {
    val tester = new Tester("metascala.features.methods.Statics")
    "helloWorld" in chk(tester.run("helloWorld", _: Int))

    "helloWorld2" in chk(tester.run("helloWorld2", _: Int, _: Int))
    "tailFactorial" in chk(tester.run("tailFactorial", _: Int))(Seq(2, 5, 10, 20, 50))
    "fibonacci" in chk(tester.run("fibonacci", _: Int))(Seq(2, 5, 10))
    "callAtPhiBoundary" in tester.run("callAtPhiBoundary", 0)
  }
  "natives" - {
    val tester = new Tester("metascala.features.methods.Natives")
    "intBitsToFloat" in chk(tester.run("intBitsToFloat", _: Int))
    "currentTimeMillis" in tester.run("currentTimeMillis")
    "inheritedNative" in tester.run("inheritedNative")
    "arrayCopy" in tester.run("arrayCopy")
  }
  "objects" - {
    val tester = new Tester("metascala.features.methods.Objects")
    "helloWorld" in tester.run("helloWorld", 5)
    "stringEquals" in chk(tester.run("stringEquals", _: Int, _: String))(
      Seq(0),
      Seq("0")
    )
    "inheritance" in tester.run("inheritance", 5)
    "points" in chk(tester.run("points", _: Int))
    "points2" in chk(tester.run("points", _: Int))
  }
}

