package metascala
package features

import org.scalatest.FreeSpec

import metascala.Util
import scala.Some
import java.util.Arrays
import Gen._
class ArrayTest extends FreeSpec with Util{


  "array stuff" - {
    val buffer = new BufferLog(4000)
    val tester = new Tester("metascala.features.arrays.ArrayStuff", buffer)
    "makeIntArray" in chk(tester.run("makeIntArray", _: Int))(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    "makeFloatArray" in tester.run("makeFloatArray")
    "makeStringArray" in {
      try tester.run("makeStringArray") catch {case x =>
        buffer.lines.foreach(println)
        throw x
      }
    }
    "arrayLength" in tester.run("arrayLength")
    "arraySet" in tester.run("arraySet")
    "arrayGet" in tester.run("arrayGet")
    "bubbleSort" in chk{ src: Seq[Int] =>
      tester.runC("bubbleSort", Seq(src.toArray))
    }(Seq(
      Seq(0, 1, 2, 3, 4, 5, 6, 7),
      Seq(7, 6, 5, 4, 3, 2, 1, 0),
      Seq(0, 1, 2, 3, 4, 5, 6, 7),
      Seq.fill(10)(util.Random.nextInt()),
      Seq.fill(20)(util.Random.nextInt())
    ))
  }
  "multi dim arrays" - {
    val buffer = new BufferLog(4000)
    val tester = new Tester("metascala.features.arrays.MultiDimArrays", buffer)
    "make2D" in {
      try
      chk(tester.run("make2D", _: Int, _: Int))(Seq(0, 1, 2), Seq(0, 1, 2))
      catch {case x =>
        buffer.lines.foreach(println)
        throw x
      }
    }
    "make3D" in chk(tester.run("make3D", _: Int, _: Int, _ : Int))(Seq(0, 1, 2), Seq(0, 1, 2), Seq(0, 1, 2))
    "getAndSet" in tester.run("getAndSet")
  }

}

