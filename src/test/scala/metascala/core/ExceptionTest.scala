package metascala.core

import utest._

import metascala.TestUtil._
object ExceptionTest extends utest.TestSuite{
  def tests = Tests {
    val tester = new Tester("metascala.features.exceptions.Exceptions")

    "throwCatch" - Seq(-1, 0, 1, 2).foreach(tester.run("throwCatch", _: Int))
    "multiCatch" - Seq(0, 1, 2, 3, 4).foreach(tester.run("multiCatch", _: Int))
    "nullPointer" - Seq("omg", null).foreach(tester.run("nullPointer", _: Object))
    "arrayIndexOutOfBounds" - {
      Seq(-1, 0, 1, 2, 3, 4, 5, 100).foreach(tester.run("arrayIndexOutOfBounds", _: Int))
    }
  }
}

