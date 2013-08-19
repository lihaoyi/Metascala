package metascala.features

import org.scalatest.FreeSpec

import metascala.{BufferLog, Gen, Util}
import Gen.chk
class ExceptionTest extends FreeSpec {
  import Util._
  "if else" - {
    val tester = new Tester("metascala.features.exceptions.Exceptions")

    "throwCatch" in chk(tester.run("throwCatch", _: Int))(Seq(-1, 0, 1, 2))
    "multiCatch" in chk(tester.run("multiCatch", _: Int))(Seq(0, 1, 2, 3, 4))
    "nullPointer" in chk(tester.run("nullPointer", _: Object))(Seq("omg", null))
    "arrayIndexOutOfBounds" in {
      chk(tester.run("arrayIndexOutOfBounds", _: Int))(Seq(-1, 0, 1, 2, 3, 4, 5, 100))
    }
  }

}

