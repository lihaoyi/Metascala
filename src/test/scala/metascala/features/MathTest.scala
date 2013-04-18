package metascala
package features


import org.scalatest.FreeSpec
import Gen._
import metascala.Util
import util.Random



class MathTest extends FreeSpec with Util{

  implicit val intAll10 = 10 ** Gen.intAll
  implicit val floatAll110 = 10 ** Gen.floatAll
  implicit val longAll10 = 10 ** Gen.longAll
  implicit val doubleAll110 = 10 ** Gen.doubleAll
  "single precision" - {
    val tester = new Tester("metascala.features.math.HelloMath")
    "hello math" - {

      "imain" in tester.run("imain")
      "fmain" in tester.run("fmain")
    }
    "basic math" - {
      "int" - {
        "ineg" in chk(tester.run("ineg", _: Int))
        "iadd" in chk(tester.run("iadd", _: Int, _: Int))
        "isub" in chk(tester.run("isub", _: Int, _: Int))
        "imul" in chk(tester.run("imul", _: Int, _: Int))
        "idiv" in chk(tester.run("idiv", _: Int, _: Int))
        "imod" in chk(tester.run("imod", _: Int, _: Int))
      }
      "float" - {
        "fneg" in chk(tester.run("fneg", _: Float))
        "fadd" in chk(tester.run("fadd", _: Float, _: Float))
        "fsub" in chk(tester.run("fsub", _: Float, _: Float))
        "fmul" in chk(tester.run("fmul", _: Float, _: Float))
        "fdiv" in chk(tester.run("fdiv", _: Float, _: Float))
        "fmod" in chk(tester.run("fmod", _: Float, _: Float))
      }
      "more int stuff" - {
        "ishl" in chk(tester.run("ishl", _: Int, _: Int))(10 ** Gen.intAll, 10 ** Gen.int(5))
        "ishr" in chk(tester.run("ishr", _: Int, _: Int))(10 ** Gen.intAll, 10 ** Gen.int(5))
        "iushr" in chk(tester.run("iushr", _: Int, _: Int))(10 ** Gen.intAll, 10 ** Gen.int(5))
        "iand" in chk(tester.run("iand", _: Int, _: Int))
        "ior" in chk(tester.run("ior", _: Int, _: Int))
        "ixor" in chk(tester.run("ixor", _: Int, _: Int))
      }

    }
  }
  "double precision" - {
    val tester = new Tester("metascala.features.math.HelloLongs")
    "hello longs" - {

      "lmain" in tester.run("lmain")
      "dmain" in tester.run("dmain")
    }
    "basic math" - {
      "long" - {
        "lneg" in chk(tester.run("lneg", _: Long))
        "ladd" in chk(tester.run("ladd", _: Long, _: Long))
        "lsub" in chk(tester.run("lsub", _: Long, _: Long))
        "lmul" in chk(tester.run("lmul", _: Long, _: Long))
        "ldiv" in chk(tester.run("ldiv", _: Long, _: Long))
        "lmod" in chk(tester.run("lmod", _: Long, _: Long))
      }
      "double" - {
        "dneg" in chk(tester.run("dneg", _: Double))
        "dadd" in chk(tester.run("dadd", _: Double, _: Double))
        "dsub" in chk(tester.run("dsub", _: Double, _: Double))
        "dmul" in chk(tester.run("dmul", _: Double, _: Double))
        "ddiv" in chk(tester.run("ddiv", _: Double, _: Double))
        "dmod" in chk(tester.run("dmod", _: Double, _: Double))
      }

      "more long stuff" - {
        "lshl" in chk(tester.run("lshl", _: Long, _: Long))(10 ** Gen.longAll, 10 ** Gen.long(6))
        "lshr" in chk(tester.run("lshr", _: Long, _: Long))(10 ** Gen.longAll, 10 ** Gen.long(6))
        "lushr" in chk(tester.run("lushr", _: Long, _: Long))(10 ** Gen.longAll, 10 ** Gen.long(6))
        "land" in chk(tester.run("land", _: Long, _: Long))
        "lor" in chk(tester.run("lor", _: Long, _: Long))
        "lxor" in chk(tester.run("lxor", _: Long, _: Long))
      }
    }
  }

  "combined" - {
    val tester = new Tester("metascala.features.math.Combined")
    "hmsToDays" in
      chk(tester.run("hmsToDays", _: Double, _: Double, _: Double))(
        Seq.fill(0)(Random.nextInt(24).toDouble),
        Seq.fill(0)(Random.nextInt(60).toDouble),
        Seq.fill(0)(Random.nextInt(60).toDouble)
      )


  }


}

