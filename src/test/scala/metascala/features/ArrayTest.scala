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
    "makeStringArray" in tester.run("makeStringArray")
    "longArrayOps" in chk(tester.run("longArrayOps", _: Int))(Seq(0, 1, 2))
    "doubleArrayOps" in tester.run("doubleArrayOps", Array(2.1, 2.72))

    "arrayLength" in tester.run("arrayLength")
    "arraySet" in tester.run("arraySet")
    "arrayGet" in tester.run("arrayGet")
    "getSet" in tester.run("getSet")
    "bubbleSort" in chk{ src: Array[Int] =>
      tester.run("bubbleSort", src.clone())
    }(Seq(
      Array(0, 1, 2, 3, 4, 5, 6, 7),
      Array(7, 6, 5, 4, 3, 2, 1, 0),
      Array(0, 1, 2, 3, 4, 5, 6, 7),
      Array.fill(10)(util.Random.nextInt()),
      Array.fill(20)(util.Random.nextInt())
    ))
  }
  "multi dim arrays" - {
    val buffer = new BufferLog(4000)
    val tester = new Tester("metascala.features.arrays.MultiDimArrays", buffer)
    "make2D" in chk(tester.run("make2D", _: Int, _: Int))(Seq(0, 1, 2), Seq(0, 1, 2))
    "make3D" in chk(tester.run("make3D", _: Int, _: Int, _ : Int))(Seq(0, 1, 2), Seq(0, 1, 2), Seq(0, 1, 2))
    "getAndSet" in tester.run("getAndSet")
  }
}