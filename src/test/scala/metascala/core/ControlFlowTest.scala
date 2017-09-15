package metascala
package core
import TestUtil._
import utest._
import Gen.chk
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
      "basicFor" - chk(tester.run("basicFor", _: Int))(Seq.fill(0)(Gen.int(256)))
      "nullWhile" - tester.run("nullWhile", 100)
      "basicWhile" - chk(tester.run("basicWhile", _: Int))(Seq.fill(0)(Gen.int(256)))
      "sqrtFinder" - chk(tester.run("sqrtFinder", _: Double))(Seq.fill(10)(Math.random() * 1000))
    }
    "switches" - {
      val tester = new Tester("metascala.features.controlflow.Switches")
      "smallSwitch" - chk(tester.run("smallSwitch", _: Int))(Seq(0, 1, 2))
      "bigDenseSwitch" - chk(tester.run("bigDenseSwitch", _: Int))(0 to 30)
      "bigSparseSwitch" - chk(tester.run("bigSparseSwitch", _: Int))((0 to 23).map(x => Math.pow(2, x).toInt))
      "charSwitch" - chk(tester.run("charSwitch", _: Char))('a' to 'k')
      "byteSwitch" - chk(tester.run("byteSwitch", _: Byte))((0 to 8).map(x => Math.pow(2, x).toByte))
      "stringSwitch" - chk(tester.run("stringSwitch", _: Int))(Seq(1))
      "stringSwitchTwo" - chk(tester.run("stringSwitchTwo", _: String))(Seq("omg", "wtf", "bbq", "lol"))
    }
  }
}

