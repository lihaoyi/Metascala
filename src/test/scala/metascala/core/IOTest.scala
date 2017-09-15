package metascala
package core

import utest._
import metascala.Gen._

import scala.util.{Failure, Try}
import metascala.{BufferLog, Gen, TestUtil}
import metascala.TestUtil.SingleClassVM
import metascala.util.UncaughtVmException

object IOTest extends utest.TestSuite {

  import TestUtil._

  implicit val intAll10 = 10 ** Gen.intAll
  def tests = Tests {
    val tester = new VM()
    "primitives" - {
      "retInt" - tester.testFunc(() => 1337)
      "retDouble" - tester.testFunc(() => 3.1337)
      "argInt" - tester.testFunc((i: Int) => i)(10)
      "argDouble" - tester.testFunc((i: Double) => i)(10.01)
      "multiArgD" - tester.testFunc((i: Int, d: Double) => d)(27, 3.14)
      "multiArgI" - tester.testFunc((i: Int, d: Double) => i)(27, 3.14)

      "stringLiteral" - tester.testFunc(() => "omgwtfbbq")
      "strings" - tester.testFunc((s: String) => s + "a")("mooo")
      "nullReturn" - tester.testFunc(() => null)
      "arrayObj" - tester.testFunc { () =>
        val arr = new Array[Int](3)
        arr(0) = 1
        arr(1) = 2
        arr(2) = 4
        arr
      }

    }
    "exceptions" - {
      "runtime" - {
        val svmRes = Try(tester.testFunc { () =>
          val s: String = null;
          s.charAt(0);
          10
        })

        val Failure(u@UncaughtVmException(wrapped)) = svmRes

      }
    }

  }

}