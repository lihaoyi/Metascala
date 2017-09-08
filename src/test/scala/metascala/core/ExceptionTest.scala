package metascala.core

import utest._

import metascala.{BufferLog, Gen, TestUtil}
import Gen.chk
import TestUtil._
object ExceptionTest extends utest.TestSuite{
  def tests = this {
    val tester = new Tester("metascala.features.exceptions.Exceptions")

    "throwCatch" - chk(tester.run("throwCatch", _: Int))(Seq(-1, 0, 1, 2))
    "multiCatch" - chk(tester.run("multiCatch", _: Int))(Seq(0, 1, 2, 3, 4))
    "nullPointer" - chk(tester.run("nullPointer", _: Object))(Seq("omg", null))
    "arrayIndexOutOfBounds" - {
      chk(tester.run("arrayIndexOutOfBounds", _: Int))(Seq(-1, 0, 1, 2, 3, 4, 5, 100))
    }
  }
}

