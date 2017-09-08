package metascala
package core

import org.scalatest.FreeSpec

import metascala.{Gen, TestUtil}
import metascala.Gen._
import java.awt.Point

class StaticMethodTest extends FreeSpec {
  import TestUtil._

  implicit val intAll10 = 10 ** Gen.intAll

  "static" - {
    val tester = new Tester("metascala.features.methods.Statics")
    "helloWorld" in chk(tester.run("helloWorld", _: Int))

    "helloWorld2" in chk(tester.run("helloWorld2", _: Int, _: Int))
    "tailFactorial" in chk(tester.run("tailFactorial", _: Int))(Seq(2, 5, 10, 20, 50))
    "fibonacci" in chk(tester.run("fibonacci", _: Int))(Seq(2, 5, 10))
    "callAtPhiBoundary" in tester.run("callAtPhiBoundary", 0)
  }
}

