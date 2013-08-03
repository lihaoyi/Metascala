package metascala.full

import org.scalatest.FreeSpec

import metascala.Gen._
import util.{Failure, Try}
import metascala.{UncaughtVmException, BufferLog, Gen, Util}
import metascala.Util.SingleClassVM

class JavaLibTest extends FreeSpec with Util{


  implicit val intAll10 = 10 ** Gen.intAll

  "stuff" - {
    val tester = new Tester("metascala.full.JavaLib")
    "sorting" in tester.run("sorting")
    "collections" in tester.run("collections", 10)
    "sudoku" in tester.run("sudoku")
    "bigInteger" in tester.run("bigInteger")
    "regex" in tester.run("regex")
    "atomicBooleans" in tester.run("atomicBooleans")
    "atomicIntegers" in tester.run("atomicIntegers")
    "atomicLongs" in tester.run("atomicLongs")
    "randoms" in tester.run("randoms")
  }
}

