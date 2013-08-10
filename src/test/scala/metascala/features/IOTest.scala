package metascala.features

import org.scalatest.FreeSpec

import metascala.Gen._
import util.{Failure, Try}
import metascala.{UncaughtVmException, BufferLog, Gen, Util}
import metascala.Util.SingleClassVM

class IOTest extends FreeSpec with Util{


  implicit val intAll10 = 10 ** Gen.intAll

  "primitives" - {
    val buffer = new BufferLog(4000)
    val tester = new Tester("metascala.features.IO", buffer)
    "retInt" in tester.run("retInt")
    "retDouble" in tester.run("retDouble")
    "argInt" in tester.run("argInt", 10)
    "argDouble" in tester.run("argDouble", 10.01)
    "multiArgD" in tester.run("multiArgD", 27, 3.14)
    "multiArgI" in tester.run("multiArgI", 27, 3.14)

    "stringLiteral" in tester.run("stringLiteral")
    "strings" in tester.run("strings", "mooo")
    "nullReturn" in tester.run("nullReturn")
    "arrayObj" in tester.run("arrayObj")

  }
  "exceptions" -{
    val tester = new SingleClassVM("metascala.features.IO", x => ())
    "runtime" in {
      val svmRes = Try(tester.run("runtime"))

      val Failure(u @ UncaughtVmException(wrapped)) = svmRes

    }
  }

}

