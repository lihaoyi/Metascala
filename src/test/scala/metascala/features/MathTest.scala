package metascala
package features


import org.scalatest.FreeSpec
import Gen._
import metascala.Util
import util.Random



class MathTest extends FreeSpec {
  import Util._
  implicit def intAll10 = 10 ** Gen.intAll
  implicit def floatAll110 = 10 ** Gen.floatAll
  implicit def longAll10 = 10 ** Gen.longAll
  implicit def doubleAll110 = 10 ** Gen.doubleAll
  val tester = new VM()

  "single precision" - {
    "hello math" - {
      "imain" in tester.test{ 1337 }
      "fmain" in tester.test{ 1.337f }
    }
    "basic math" - {
      "int" - {
        "ineg" in chk(tester.testFunc( -(_: Int) ) _)
        "iadd" in chk(tester.testFunc( (_: Int) + (_: Int) ) _)
        "isub" in chk(tester.testFunc( (_: Int) - (_: Int) ) _)
        "imul" in chk(tester.testFunc( (_: Int) * (_: Int) ) _)
        "idiv" in chk(tester.testFunc( (_: Int) / (_: Int) ) _)
        "imod" in chk(tester.testFunc( (_: Int) % (_: Int) ) _)
      }
      "float" - {
        "fneg" in chk(tester.testFunc( -(_: Float) ) _)
        "fadd" in chk(tester.testFunc( (_: Float) + (_: Float) ) _)
        "fsub" in chk(tester.testFunc( (_: Float) - (_: Float) ) _)
        "fmul" in chk(tester.testFunc( (_: Float) * (_: Float) ) _)
        "fdiv" in chk(tester.testFunc( (_: Float) / (_: Float) ) _)
        "fmod" in chk(tester.testFunc( (_: Float) % (_: Float) ) _)
      }
      "more int stuff" - {
        "ishl" in chk(tester.testFunc( (_: Int) << (_: Int)) _)(10 ** Gen.intAll, 10 ** Gen.int(5))
        "ishr" in chk(tester.testFunc( (_: Int) >> (_: Int)) _)(10 ** Gen.intAll, 10 ** Gen.int(5))
        "iushr" in chk(tester.testFunc( (_: Int) >>> (_: Int)) _)(10 ** Gen.intAll, 10 ** Gen.int(5))
        "iand" in chk(tester.testFunc( (_: Int) & (_: Int)) _)
        "ior" in chk(tester.testFunc( (_: Int) | (_: Int)) _)
        "ixor" in chk(tester.testFunc( (_: Int) ^ (_: Int)) _)
      }
    }
  }
  "double precision" - {

    "hello math" - {
      "lmain" in tester.testFunc{ () => 313373133731337L }
      "dmain" in tester.testFunc{ () => 31.337 }
    }
    "basic math" - {
      "long" - {
        "lneg" in chk(tester.testFunc( -(_: Long) ) _)
        "ladd" in chk(tester.testFunc( (_: Long) + (_: Long) ) _)
        "lsub" in chk(tester.testFunc( (_: Long) - (_: Long) ) _)
        "lmul" in chk(tester.testFunc( (_: Long) * (_: Long) ) _)
        "ldiv" in chk(tester.testFunc( (_: Long) / (_: Long) ) _)
        "lmod" in chk(tester.testFunc( (_: Long) % (_: Long) ) _)
      }
      "double" - {
        "dneg" in chk(tester.testFunc( -(_: Double) ) _)
        "dadd" in chk(tester.testFunc( (_: Double) + (_: Double) ) _)
        "dsub" in chk(tester.testFunc( (_: Double) - (_: Double) ) _)
        "dmul" in chk(tester.testFunc( (_: Double) * (_: Double) ) _)
        "ddiv" in chk(tester.testFunc( (_: Double) / (_: Double) ) _)
        "dmod" in chk(tester.testFunc( (_: Double) % (_: Double) ) _)
      }
      "more long stuff" - {
        "ishl" in chk(tester.testFunc( (_: Long) << (_: Int)) _)(10 ** Gen.longAll, 10 ** Gen.int(5))
        "ishr" in chk(tester.testFunc( (_: Long) >> (_: Int)) _)(10 ** Gen.longAll, 10 ** Gen.int(5))
        "iushr" in chk(tester.testFunc( (_: Long) >>> (_: Int)) _)(10 ** Gen.longAll, 10 ** Gen.int(5))
        "iand" in chk(tester.testFunc( (_: Long) & (_: Long)) _)
        "ior" in chk(tester.testFunc( (_: Long) | (_: Long)) _)
        "ixor" in chk(tester.testFunc( (_: Long) ^ (_: Long)) _)
      }
    }
  }

  "combined" - {
    "hmsToDays" in
      chk(tester.testFunc(
        (h: Double, m: Double, s: Double) => (((h*60)+m)*60+s) / 86400
      ) _)(
        Seq.fill(0)(Random.nextInt(24).toDouble),
        Seq.fill(0)(Random.nextInt(60).toDouble),
        Seq.fill(0)(Random.nextInt(60).toDouble)
      )


  }


}

