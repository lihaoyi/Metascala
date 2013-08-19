package metascala
package features

import org.scalatest.FreeSpec

import metascala.Gen._
import util.{Failure, Try}
import metascala.{UncaughtVmException, BufferLog, Gen, Util}
import metascala.Util.SingleClassVM

class IOTest extends FreeSpec {

  import Util._
  implicit val intAll10 = 10 ** Gen.intAll
  val tester = new VM()
  "primitives" - {
    "retInt" in tester.testFunc(() => 1337)
    "retDouble" in tester.testFunc(() => 3.1337)
    "argInt" in tester.testFunc((i: Int) => i)(10)
    "argDouble" in tester.testFunc((i: Double) => i)(10.01)
    "multiArgD" in tester.testFunc((i: Int, d: Double) => d)(27, 3.14)
    "multiArgI" in tester.testFunc((i: Int, d: Double) => i)(27, 3.14)

    "stringLiteral" in tester.testFunc(() => "omgwtfbbq")
    "strings" in tester.testFunc((s: String) => s + "a")("mooo")
    "nullReturn" in tester.testFunc(() => null)
    "arrayObj" in tester.testFunc{() =>
      val arr = new Array[Int](3)
      arr(0) = 1
      arr(1) = 2
      arr(2) = 4
      arr
    }

  }
  "exceptions" -{
    "runtime" in {
      val svmRes = Try(tester.testFunc{() =>
        val s: String = null;
        s.charAt(0);
        10
      })

      val Failure(u @ UncaughtVmException(wrapped)) = svmRes

    }
  }

}

