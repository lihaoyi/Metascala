package svm
package helloworld


import org.scalatest.FreeSpec

import svm.Util
import Gen.check
class ControlFlow extends FreeSpec with Util{


  "if else" - {
    val tester = new Tester("svm.helloworld.controlflow.IfElse")
    "basicIf" in tester.run("basicIf")
    "ifElseIf" in tester.run("ifElseIf")
    "ifElseIfBig" in tester.run("ifElseIfBig")
  }
  "loops" - {
    val tester = new Tester("svm.helloworld.controlflow.Loops")

    "nullFor" in tester.run("nullFor", 100)
    "basicFor" in check(tester.run("basicFor", _: Int))(Gen.int(256))
    "nullWhile" in tester.run("nullWhile", 100)
    "basicWhile" in check(tester.run("basicWhile", _: Int))(Gen.int(256))
  }


}

