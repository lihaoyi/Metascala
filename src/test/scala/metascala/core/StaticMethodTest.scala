package metascala
package core

import utest._

import metascala.{Gen, TestUtil}
import metascala.Gen._
import java.awt.Point

class StaticMethodTest extends utest.TestSuite{
  import TestUtil._

  implicit val intAll10 = 10 ** Gen.intAll
  def tests = this {
    val tester = new Tester("metascala.features.methods.Statics")
    "helloWorld" - chk(tester.run("helloWorld", _: Int))

    "helloWorld2" - chk(tester.run("helloWorld2", _: Int, _: Int))
    "tailFactorial" - chk(tester.run("tailFactorial", _: Int))(Seq(2, 5, 10, 20, 50))
    "fibonacci" - chk(tester.run("fibonacci", _: Int))(Seq(2, 5, 10))
    "callAtPhiBoundary" - tester.run("callAtPhiBoundary", 0)
  }
}

