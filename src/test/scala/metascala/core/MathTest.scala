package metascala
package core


import utest._
import Gen._
import metascala.TestUtil
import scala.util.Random



object MathTest extends utest.TestSuite{
  import TestUtil._
  implicit def intAll10 = 10 ** Gen.intAll
  implicit def floatAll110 = 10 ** Gen.floatAll
  implicit def longAll10 = 10 ** Gen.longAll
  implicit def doubleAll110 = 10 ** Gen.doubleAll
  val tester = new VM()
  def tests = Tests {
    "single precision" - {
      "hello math" - {
        "imain" - tester.test {
          1337
        }
        "fmain" - tester.test {
          1.337f
        }
      }
      "basic math" - {
        "int" - {
          "ineg" - chk(tester.testFunc(-(_: Int)) _)
          "iadd" - chk(tester.testFunc((_: Int) + (_: Int)) _)
          "isub" - chk(tester.testFunc((_: Int) - (_: Int)) _)
          "imul" - chk(tester.testFunc((_: Int) * (_: Int)) _)
          "idiv" - chk(tester.testFunc((_: Int) / (_: Int)) _)
          "imod" - chk(tester.testFunc((_: Int) % (_: Int)) _)
        }
        "float" - {
          "fneg" - chk(tester.testFunc(-(_: Float)) _)
          "fadd" - chk(tester.testFunc((_: Float) + (_: Float)) _)
          "fsub" - chk(tester.testFunc((_: Float) - (_: Float)) _)
          "fmul" - chk(tester.testFunc((_: Float) * (_: Float)) _)
          "fdiv" - chk(tester.testFunc((_: Float) / (_: Float)) _)
          "fmod" - chk(tester.testFunc((_: Float) % (_: Float)) _)
        }
        "more int stuff" - {
          "ishl" - chk(tester.testFunc((_: Int) << (_: Int)) _)(10 ** Gen.intAll, 10 ** Gen.int(5))
          "ishr" - chk(tester.testFunc((_: Int) >> (_: Int)) _)(10 ** Gen.intAll, 10 ** Gen.int(5))
          "iushr" - chk(tester.testFunc((_: Int) >>> (_: Int)) _)(10 ** Gen.intAll, 10 ** Gen.int(5))
          "iand" - chk(tester.testFunc((_: Int) & (_: Int)) _)
          "ior" - chk(tester.testFunc((_: Int) | (_: Int)) _)
          "ixor" - chk(tester.testFunc((_: Int) ^ (_: Int)) _)
        }
      }
    }
    "double precision" - {

      "hello math" - {
        "lmain" - tester.testFunc { () => 313373133731337L }
        "dmain" - tester.testFunc { () => 31.337 }
      }
      "basic math" - {
        "long" - {
          "lneg" - chk(tester.testFunc(-(_: Long)) _)
          "ladd" - chk(tester.testFunc((_: Long) + (_: Long)) _)
          "lsub" - chk(tester.testFunc((_: Long) - (_: Long)) _)
          "lmul" - chk(tester.testFunc((_: Long) * (_: Long)) _)
          "ldiv" - chk(tester.testFunc((_: Long) / (_: Long)) _)
          "lmod" - chk(tester.testFunc((_: Long) % (_: Long)) _)
        }
        "double" - {
          "dneg" - chk(tester.testFunc(-(_: Double)) _)
          "dadd" - chk(tester.testFunc((_: Double) + (_: Double)) _)
          "dsub" - chk(tester.testFunc((_: Double) - (_: Double)) _)
          "dmul" - chk(tester.testFunc((_: Double) * (_: Double)) _)
          "ddiv" - chk(tester.testFunc((_: Double) / (_: Double)) _)
          "dmod" - chk(tester.testFunc((_: Double) % (_: Double)) _)
        }
        "more long stuff" - {
          "ishl" - chk(tester.testFunc((_: Long) << (_: Int)) _)(10 ** Gen.longAll, 10 ** Gen.int(5))
          "ishr" - chk(tester.testFunc((_: Long) >> (_: Int)) _)(10 ** Gen.longAll, 10 ** Gen.int(5))
          "iushr" - chk(tester.testFunc((_: Long) >>> (_: Int)) _)(10 ** Gen.longAll, 10 ** Gen.int(5))
          "iand" - chk(tester.testFunc((_: Long) & (_: Long)) _)
          "ior" - chk(tester.testFunc((_: Long) | (_: Long)) _)
          "ixor" - chk(tester.testFunc((_: Long) ^ (_: Long)) _)
        }
      }
    }

    "combined" - {
      "hmsToDays" - {
        chk(tester.testFunc(
          (h: Double, m: Double, s: Double) => (((h * 60) + m) * 60 + s) / 86400
        ) _)(
          Seq.fill(0)(Random.nextInt(24).toDouble),
          Seq.fill(0)(Random.nextInt(60).toDouble),
          Seq.fill(0)(Random.nextInt(60).toDouble)
        )
      }


    }

  }
}

