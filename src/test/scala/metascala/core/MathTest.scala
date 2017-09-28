package metascala
package core
import utest._

import metascala.TestUtil._
import scala.util.Random



object MathTest extends utest.TestSuite{
  implicit def intAll10 = Iterator.fill(10)(Random.nextInt())
  implicit def longAll10 = Iterator.fill(10)(Random.nextLong())
  implicit def floatAll10 = Iterator.fill(10)(Random.nextFloat())
  implicit def doubleAll10 = Iterator.fill(10)(Random.nextDouble())
  def checkBinaryFunc[T1, T2](test: (T1, T2) => Unit)
                             (implicit gen1: Iterator[T1], gen2: Iterator[T2]) = {
    for ((i, j) <- gen1 zip gen2){
      test(i, j)
    }
  }

  val tester = new VM()
  def tests = Tests {
    "single precision" - {
      "hello math" - {
        "imain" - tester.testSafe {
          1337
        }
        "fmain" - tester.testSafe{
          1.337f
        }
      }
      "basic math" - {
        "int" - {
          "ineg" - intAll10.foreach(x => tester.test(-x))
          "iadd" - checkBinaryFunc(tester.testFunc((_: Int) + (_: Int)))
          "isub" - checkBinaryFunc(tester.testFunc((_: Int) - (_: Int)))
          "imul" - checkBinaryFunc(tester.testFunc((_: Int) * (_: Int)))
          "idiv" - checkBinaryFunc(tester.testFunc((_: Int) / (_: Int)))
          "imod" - checkBinaryFunc(tester.testFunc((_: Int) % (_: Int)))
        }
        "float" - {
          "fneg" - floatAll10.foreach(x => tester.test(-x))
          "fadd" - checkBinaryFunc(tester.testFunc((_: Float) + (_: Float)))
          "fsub" - checkBinaryFunc(tester.testFunc((_: Float) - (_: Float)))
          "fmul" - checkBinaryFunc(tester.testFunc((_: Float) * (_: Float)))
          "fdiv" - checkBinaryFunc(tester.testFunc((_: Float) / (_: Float)))
          "fmod" - checkBinaryFunc(tester.testFunc((_: Float) % (_: Float)))
        }
        "more int stuff" - {
          "ishl" - checkBinaryFunc(tester.testFunc((_: Int) << (_: Int)))(implicitly, Iterator.fill(10)(Random.nextInt(5)))
          "ishr" - checkBinaryFunc(tester.testFunc((_: Int) >> (_: Int)))(implicitly, Iterator.fill(10)(Random.nextInt(5)))
          "iushr" - checkBinaryFunc(tester.testFunc((_: Int) >>> (_: Int)))(implicitly, Iterator.fill(10)(Random.nextInt(5)))
          "iand" - checkBinaryFunc(tester.testFunc((_: Int) & (_: Int)))
          "ior" - checkBinaryFunc(tester.testFunc((_: Int) | (_: Int)))
          "ixor" - checkBinaryFunc(tester.testFunc((_: Int) ^ (_: Int)))
        }
      }
    }
    "double precision" - {

      "hello math" - {
        "lmain" - tester.test { 313373133731337L }
        "dmain" - tester.test { 31.337 }
      }
      "basic math" - {
        "long" - {
          "lneg" - longAll10.foreach(x => tester.test(-x))
          "ladd" - checkBinaryFunc(tester.testFunc((_: Long) + (_: Long)))
          "lsub" - checkBinaryFunc(tester.testFunc((_: Long) - (_: Long)))
          "lmul" - checkBinaryFunc(tester.testFunc((_: Long) * (_: Long)))
          "ldiv" - checkBinaryFunc(tester.testFunc((_: Long) / (_: Long)))
          "lmod" - checkBinaryFunc(tester.testFunc((_: Long) % (_: Long)))
        }
        "double" - {
          "dneg" - doubleAll10.foreach(x => tester.test(-x))
          "dadd" - checkBinaryFunc(tester.testFunc((_: Double) + (_: Double)))
          "dsub" - checkBinaryFunc(tester.testFunc((_: Double) - (_: Double)))
          "dmul" - checkBinaryFunc(tester.testFunc((_: Double) * (_: Double)))
          "ddiv" - checkBinaryFunc(tester.testFunc((_: Double) / (_: Double)))
          "dmod" - checkBinaryFunc(tester.testFunc((_: Double) % (_: Double)))
        }
        "more long stuff" - {
          "ishl" - checkBinaryFunc(tester.testFunc((_: Long) << (_: Int)))(implicitly, Iterator.fill(10)(Random.nextInt(5)))
          "ishr" - checkBinaryFunc(tester.testFunc((_: Long) >> (_: Int)))(implicitly, Iterator.fill(10)(Random.nextInt(5)))
          "iushr" - checkBinaryFunc(tester.testFunc((_: Long) >>> (_: Int)))(implicitly, Iterator.fill(10)(Random.nextInt(5)))
          "iand" - checkBinaryFunc(tester.testFunc((_: Long) & (_: Long)))
          "ior" - checkBinaryFunc(tester.testFunc((_: Long) | (_: Long)))
          "ixor" - checkBinaryFunc(tester.testFunc((_: Long) ^ (_: Long)))
        }
      }
    }

    "combined" - {
      "hmsToDays" - {
        for{
          h <- Seq.fill(0)(Random.nextInt(24).toDouble)
          m <- Seq.fill(0)(Random.nextInt(60).toDouble)
          s <- Seq.fill(0)(Random.nextInt(60).toDouble)
        } tester.test((((h * 60) + m) * 60 + s) / 86400)
      }


    }

  }
}

