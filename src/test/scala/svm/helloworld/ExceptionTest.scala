package svm.helloworld

import org.scalatest.FreeSpec

import svm.{Gen, Util}
import Gen.check
class ExceptionTest extends FreeSpec with Util{


  "if else" - {
    val tester = new Tester("svm.helloworld.exceptions.Exceptions")

    "throwCatch" in tester.run("throwCatch")
    "multiCatch" in check(tester.run("multiCatch", _: Int), 5)(Seq(0, 1, 2, 3, 4))
    "nullPointer" in check(tester.run("nullPointer", _: Object), 2)(Seq("omg", null))
    "arrayIndexOutOfBounds" in check(tester.run("arrayIndexOutOfBounds", _: Int), 5)(Seq(-1, 0, 1, 2, 3, 4))
  }

}

