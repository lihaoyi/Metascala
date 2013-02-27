package svm
package helloworld


import org.scalatest.FreeSpec
import Gen._
import svm.Util
import util.Random



class MathTest extends FreeSpec with Util{

  implicit val intAll10 = 10 ** Gen.intAll
  implicit val floatAll110 = 10 ** Gen.floatAll
  implicit val longAll10 = 10 ** Gen.longAll
  implicit val doubleAll110 = 10 ** Gen.doubleAll
  "single precision" - {
    val tester = new Tester("svm.helloworld.math.HelloMath")
    "hello math" - {

      "imain" in tester.run("imain")
      "fmain" in tester.run("fmain")
      "amain" in tester.run("amain")
    }
    "basic math" - {
      "int" - {
        "ineg" in check(tester.run("ineg", _: Int))
        "iadd" in check(tester.run("iadd", _: Int, _: Int))
        "isub" in check(tester.run("isub", _: Int, _: Int))
        "imul" in check(tester.run("imul", _: Int, _: Int))
        "idiv" in check(tester.run("idiv", _: Int, _: Int))
        "imod" in check(tester.run("imod", _: Int, _: Int))
      }
      "float" - {
        "fneg" in check(tester.run("fneg", _: Float))
        "fadd" in check(tester.run("fadd", _: Float, _: Float))
        "fsub" in check(tester.run("fsub", _: Float, _: Float))
        "fmul" in check(tester.run("fmul", _: Float, _: Float))
        "fdiv" in check(tester.run("fdiv", _: Float, _: Float))
        "fmod" in check(tester.run("fmod", _: Float, _: Float))
      }
      "more int stuff" - {
        "ishl" in check(tester.run("ishl", _: Int, _: Int))(10 ** Gen.intAll, 10 ** Gen.int(5))
        "ishr" in check(tester.run("ishr", _: Int, _: Int))(10 ** Gen.intAll, 10 ** Gen.int(5))
        "iushr" in check(tester.run("iushr", _: Int, _: Int))(10 ** Gen.intAll, 10 ** Gen.int(5))
        "iand" in check(tester.run("iand", _: Int, _: Int))
        "ior" in check(tester.run("ior", _: Int, _: Int))
        "ixor" in check(tester.run("ixor", _: Int, _: Int))
      }

    }
  }
  "double precision" - {
    val tester = new Tester("svm.helloworld.math.HelloLongs")
    "hello longs" - {

      "lmain" in tester.run("lmain")
      "dmain" in tester.run("dmain")
    }
    "basic math" - {
      "long" - {
        "lneg" in check(tester.run("lneg", _: Long))
        "ladd" in check(tester.run("ladd", _: Long, _: Long))
        "lsub" in check(tester.run("lsub", _: Long, _: Long))
        "lmul" in check(tester.run("lmul", _: Long, _: Long))
        "ldiv" in check(tester.run("ldiv", _: Long, _: Long))
        "lmod" in check(tester.run("lmod", _: Long, _: Long))
      }
      "double" - {
        "dneg" in check(tester.run("dneg", _: Double))
        "dadd" in check(tester.run("dadd", _: Double, _: Double))
        "dsub" in check(tester.run("dsub", _: Double, _: Double))
        "dmul" in check(tester.run("dmul", _: Double, _: Double))
        "ddiv" in check(tester.run("ddiv", _: Double, _: Double))
        "dmod" in check(tester.run("dmod", _: Double, _: Double))
      }

      "more long stuff" - {
        "lshl" in check(tester.run("lshl", _: Long, _: Long))(10 ** Gen.longAll, 10 ** Gen.long(6))
        "lshr" in check(tester.run("lshr", _: Long, _: Long))(10 ** Gen.longAll, 10 ** Gen.long(6))
        "lushr" in check(tester.run("lushr", _: Long, _: Long))(10 ** Gen.longAll, 10 ** Gen.long(6))
        "land" in check(tester.run("land", _: Long, _: Long))
        "lor" in check(tester.run("lor", _: Long, _: Long))
        "lxor" in check(tester.run("lxor", _: Long, _: Long))
      }
    }
  }

  "combined" - {
    val tester = new Tester("svm.helloworld.math.Combined")
    "hmsToDays" in
      check(tester.run("hmsToDays", _: Double, _: Double, _: Double))(
        Seq.fill(0)(Random.nextInt(24).toDouble),
        Seq.fill(0)(Random.nextInt(60).toDouble),
        Seq.fill(0)(Random.nextInt(60).toDouble)
      )


  }


}

