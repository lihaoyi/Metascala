package metascala
package core
import TestUtil._
import utest._
object ControlFlowTest extends utest.TestSuite {

  def tests = Tests {
    "ifElse" - {
      val tester = new Tester("metascala.features.controlflow.IfElse")
      "basicIf" - tester.run("basicIf")
      "ifNonIntZero" - tester.run("ifNonIntZero")
      "ifNonIntBinary" - tester.run("ifNonIntBinary")
      "ifElseIf" - tester.run("ifElseIf")
      "ifElseIfBig" - tester.run("ifElseIfBig")
      "mathMin" - tester.run("mathMin")
    }
    "loops" - {
      val tester = new Tester("metascala.features.controlflow.Loops")
      "nullFor" - tester.run("nullFor", 100)
      "basicFor" - Seq(0, 10, 100, 1000).foreach(tester.run("basicFor", _: Int))
      "nullWhile" - tester.run("nullWhile", 100)
      "basicWhile" - Seq(0, 10, 100, 1000).foreach(tester.run("basicWhile", _: Int))
      "sqrtFinder" - Seq.fill(10)(Math.random() * 1000).foreach(tester.run("sqrtFinder", _: Double))
    }
    "switches" - {
      val tester = new Tester("metascala.features.controlflow.Switches")
      "smallSwitch" - Seq(0, 1, 2).foreach(tester.run("smallSwitch", _: Int))
      "bigDenseSwitch" - (0 to 30).foreach(tester.run("bigDenseSwitch", _: Int))
      "bigSparseSwitch" - (0 to 23).map(x => Math.pow(2, x).toInt).foreach(tester.run("bigSparseSwitch", _: Int))
      "charSwitch" - ('a' to 'k').foreach(tester.run("charSwitch", _: Char))
      "byteSwitch" - (0 to 8).map(x => Math.pow(2, x).toByte).foreach(tester.run("byteSwitch", _: Byte))
      "stringSwitch" - Seq(-1, 0, 1, 2, 3).foreach(tester.run("stringSwitch", _: Int))
      "stringSwitchTwo" - Seq("omg", "wtf", "bbq", "lol").foreach(tester.run("stringSwitchTwo", _: String))
    }
  }
}

