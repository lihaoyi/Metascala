package svm.helloworld

import org.scalatest.FreeSpec

import svm.Util

class ExceptionTest extends FreeSpec with Util{


  "if else" - {
    val tester = new Tester("svm.helloworld.exceptions.Exceptions")

    "throwCatch" in tester.run("throwCatch")
  }

}

