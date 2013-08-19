package metascala
package features


import org.scalatest.{Tag, FreeSpec}

import metascala.Util
import Gen.chk
class ControlFlowTest extends FreeSpec {
  import Util._


  "if else" - {
    val tester = new Tester("metascala.features.controlflow.IfElse")
    "basicIf" in tester.run("basicIf")
    "ifNonIntZero" in tester.run("ifNonIntZero")
    "ifNonIntBinary" in tester.run("ifNonIntBinary")
    "ifElseIf" in tester.run("ifElseIf")
    "ifElseIfBig" in tester.run("ifElseIfBig")
    "mathMin" in tester.run("mathMin")
  }
  "loops" - {
    val tester = new Tester("metascala.features.controlflow.Loops")
    "nullFor" in tester.run("nullFor", 100)
    "basicFor" in chk(tester.run("basicFor", _: Int))(Seq.fill(0)(Gen.int(256)))
    "nullWhile" in tester.run("nullWhile", 100)
    "basicWhile" in chk(tester.run("basicWhile", _: Int))(Seq.fill(0)(Gen.int(256)))
    "sqrtFinder" in chk(tester.run("sqrtFinder", _: Double))(Seq.fill(10)(Math.random() * 1000))
  }
  "switches" - {
    val tester = new Tester("metascala.features.controlflow.Switches")
    "smallSwitch" in chk(tester.run("smallSwitch", _: Int))(Seq(0, 1, 2))
    "bigDenseSwitch" in chk(tester.run("bigDenseSwitch", _: Int))(0 to 30)
    "bigSparseSwitch" in chk(tester.run("bigSparseSwitch", _: Int))((0 to 23).map(x => Math.pow(2, x).toInt))
    "charSwitch" in chk(tester.run("charSwitch", _: Char))('a' to 'k')
    "byteSwitch" in chk(tester.run("byteSwitch", _: Byte))((0 to 8).map(x=>Math.pow(2, x).toByte))
    "stringSwitch" in chk(tester.run("stringSwitch", _: Int))(Seq(1))
    "stringSwitchTwo" in chk(tester.run("stringSwitchTwo", _: String))(Seq("omg", "wtf", "bbq" ,"lol"))
  }

}

