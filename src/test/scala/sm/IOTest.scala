package sm

import org.scalatest.FreeSpec

import sm.Gen._
import util.{Failure, Try}
import sm.UncaughtVmException

class IOTest extends FreeSpec with Util{


  implicit val intAll10 = 10 ** Gen.intAll

  "primitives" - {
    val tester = new Tester("sm.io.Primitives")
    "retInt" in tester.run("retInt")
    "retDouble" in tester.run("retDouble")
    "argInt" in tester.run("argInt", 10)
    "argDouble" in tester.run("argDouble", 10.01)
    "multiArgD" in tester.run("multiArgD", 27, 3.14)
    "multiArgI" in tester.run("multiArgI", 27, 3.14)
    "strings" in tester.run("strings", "mooo")
  }
  "exceptions" -{
    val tester = new Tester("sm.io.Exceptions")
    "runtime" in {
      val x = Try(tester.svm.invoke("sm.io.Exceptions", "runtime", Nil))

      val Failure(UncaughtVmException(_, _, _, _)) = x
    }
  }

}

