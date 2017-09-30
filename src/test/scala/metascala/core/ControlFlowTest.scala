package metascala
package core
import TestUtil._
import utest._
object ControlFlowTest extends utest.TestSuite {

  def tests = Tests {
    "ifElse" - {
      val tester = new Tester("metascala.features.controlflow.IfElse")
      "basicIf" - tester.run[Int]("basicIf")
      "ifNonIntZero" - tester.run[Int]("ifNonIntZero")
      "ifNonIntBinary" - tester.run[Int]("ifNonIntBinary")
      "ifElseIf" - tester.run[Int]("ifElseIf")
      "ifElseIfBig" - tester.run[Int]("ifElseIfBig")
      "mathMin" - tester.run[Int]("mathMin")
    }
    "loops" - {
      val tester = new Tester("metascala.features.controlflow.Loops")
      "nullFor" - tester.run[Int]("nullFor", 100)
      "basicFor" - Seq(0, 10, 100, 1000).foreach(tester.run[Int]("basicFor", _: Int))
      "nullWhile" - tester.run[Int]("nullWhile", 100)
      "basicWhile" - Seq(0, 10, 100, 1000).foreach(tester.run[Int]("basicWhile", _: Int))
      "sqrtFinder" - Seq.fill(10)(Math.random() * 1000).foreach(tester.run[Double]("sqrtFinder", _: Double))
    }
    "switches" - {
      val tester = new Tester("metascala.features.controlflow.Switches")
      "smallSwitch" - Seq(0, 1, 2).foreach(tester.run[Int]("smallSwitch", _: Int))
      "bigDenseSwitch" - (0 to 30).foreach(tester.run[Double]("bigDenseSwitch", _: Int))
      "bigSparseSwitch" - (0 to 23).map(x => Math.pow(2, x).toInt).foreach(tester.run[Double]("bigSparseSwitch", _: Int))
      "charSwitch" - ('a' to 'k').foreach(tester.run[Int]("charSwitch", _: Char))
      "byteSwitch" - (0 to 8).map(x => Math.pow(2, x).toByte).foreach(tester.run[Int]("byteSwitch", _: Byte))
      "stringSwitch" - Seq(-1, 0, 1, 2, 3).foreach(tester.run[Int]("stringSwitch", _: Int))
      "stringSwitchTwo" - Seq("omg", "wtf", "bbq", "lol").foreach(tester.run[String]("stringSwitchTwo", _: String))
    }
  }
}

