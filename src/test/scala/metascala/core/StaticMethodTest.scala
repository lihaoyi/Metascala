package metascala
package core

import utest._

class StaticMethodTest extends utest.TestSuite{
  import TestUtil._

  def randomInts = Seq.fill(10)(scala.util.Random.nextInt(10))
  def tests = Tests {

    val tester = new Tester("metascala.features.methods.Statics")
    "helloWorld" - randomInts.foreach(tester.run[Int]("helloWorld", _: Int))

    "helloWorld2" - randomInts.zip(randomInts).foreach { case (a, b) =>
      tester.run[Int]("helloWorld2", a: Int, b: Int)
    }
    "tailFactorial" - Seq(2, 5, 10, 20, 50).foreach(tester.run[Int]("tailFactorial", _: Int))
    "fibonacci" - Seq(2, 5, 10).foreach(tester.run[Int]("fibonacci", _: Int))
    "callAtPhiBoundary" - tester.run[Int]("callAtPhiBoundary", 0)
  }
}

