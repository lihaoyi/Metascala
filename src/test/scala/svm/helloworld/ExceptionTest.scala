package svm.helloworld

import org.scalatest.FreeSpec

import svm.{Gen, Util}
import Gen.chk
class ExceptionTest extends FreeSpec with Util{


  "if else" - {
    val tester = new Tester("svm.helloworld.exceptions.Exceptions")

    "throwCatch" in tester.run("throwCatch")
    "multiCatch" in chk(tester.run("multiCatch", _: Int))(Seq(0, 1, 2, 3, 4))
    "nullPointer" in chk(tester.run("nullPointer", _: Object))(Seq("omg", null))
    "arrayIndexOutOfBounds" in chk(tester.run("arrayIndexOutOfBounds", _: Int))(Seq(-1, 0, 1, 2, 3, 4))
  }

}

