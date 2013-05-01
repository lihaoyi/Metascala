package metascala.features

import org.scalatest.FreeSpec

import metascala.{BufferLog, Gen, Util}
import Gen.chk
class ExceptionTest extends FreeSpec with Util{

  val buffer = new BufferLog(3000)
  "if else" - {
    val tester = new Tester("metascala.features.exceptions.Exceptions", buffer)

    "throwCatch" in tester.run("throwCatch")
    "multiCatch" in chk(tester.run("multiCatch", _: Int))(Seq(0, 1, 2, 3, 4))
    "nullPointer" in chk(tester.run("nullPointer", _: Object))(Seq("omg", null))
    "arrayIndexOutOfBounds" in {
      try chk(tester.run("arrayIndexOutOfBounds", _: Int))(Seq(-1, 0, 1, 2, 3, 4))
      catch{ case e =>
        buffer.lines.foreach(println)
        throw e
      }
    }
  }

}

