package metascala.features

import org.scalatest.FreeSpec

import metascala.Gen._
import util.{Failure, Try}
import metascala.{UncaughtVmException, BufferLog, Gen, Util}

class IOTest extends FreeSpec with Util{


  implicit val intAll10 = 10 ** Gen.intAll

  "primitives" - {
    val buffer = new BufferLog(4000)
    val tester = new Tester("metascala.io.Primitives", buffer)
    "retInt" in tester.run("retInt")
    "retDouble" in tester.run("retDouble")
    "argInt" in tester.run("argInt", 10)
    "argDouble" in tester.run("argDouble", 10.01)
    "multiArgD" in tester.run("multiArgD", 27, 3.14)
    "multiArgI" in tester.run("multiArgI", 27, 3.14)

    "stringLiteral" in tester.run("stringLiteral")
    "strings" in tester.run("strings", "mooo")

  }
  "exceptions" -{
    val tester = new Tester("metascala.io.Exceptions")
    "runtime" in {
      val x = Try(tester.svm.invoke("metascala/io/Exceptions", "runtime", Nil))

      val Failure(UncaughtVmException(_, _, _, _)) = x
    }
  }

}

