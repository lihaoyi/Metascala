package svm
package helloworld


import org.scalatest.FreeSpec

import svm.Util
import Gen.check
class ControlFlowTest extends FreeSpec with Util{


  "if else" - {
    val tester = new Tester("svm.helloworld.controlflow.IfElse")
    "basicIf" in tester.run("basicIf")
    "ifNonIntZero" in tester.run("ifNonIntZero")
    "ifNonIntBinary" in tester.run("ifNonIntBinary")
    "ifElseIf" in tester.run("ifElseIf")
    "ifElseIfBig" in tester.run("ifElseIfBig")
  }
  "loops" - {
    val tester = new Tester("svm.helloworld.controlflow.Loops")
    "nullFor" in tester.run("nullFor", 100)
    "basicFor" in check(tester.run("basicFor", _: Int), 10)(Gen.int(256))
    "nullWhile" in tester.run("nullWhile", 100)
    "basicWhile" in check(tester.run("basicWhile", _: Int), 10)(Gen.int(256))
  }
  "switches" - {
    val tester = new Tester("svm.helloworld.controlflow.Switches")
    "smallSwitch" in check(tester.run("smallSwitch", _: Int), 3)(Seq(0, 1, 2))
    "bigDenseSwitch" in check(tester.run("bigDenseSwitch", _: Int), 31)(0 to 30)
    "bigSparseSwitch" in check(tester.run("bigSparseSwitch", _: Int), 24)((0 to 23).map(x => Math.pow(2, x).toInt))
    "charSwitch" in check(tester.run("charSwitch", _: Char), 10)('a' to 'k')
    "byteSwitch" in check(tester.run("byteSwitch", _: Byte), 9)((0 to 8).map(x=>Math.pow(2, x).toByte))
    "stringSwitch" in check(tester.run("stringSwitch", _: Int), 3)(Seq(0, 1, 2))
    "stringSwitchTwo" in check(tester.run("stringSwitchTwo", _: String), 4)(Seq("omg", "wtf", "bbq" ,"lol"))
  }


}

