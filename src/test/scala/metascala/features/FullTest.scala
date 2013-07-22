package metascala.features

import org.scalatest.FreeSpec

import metascala.Gen._
import util.{Failure, Try}
import metascala.{UncaughtVmException, BufferLog, Gen, Util}
import metascala.Util.SingleClassVM

class FullTest extends FreeSpec with Util{


  implicit val intAll10 = 10 ** Gen.intAll

  "stuff" - {
    val tester = new Tester("metascala.Full")
    "sorting" in tester.run("sorting")
    "collections" in tester.run("collections", 10)
    "sudoku" in tester.run("sudoku")
    "fasta" in tester.run("fasta")
  }
}

