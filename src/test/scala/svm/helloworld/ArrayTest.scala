package svm
package helloworld

import org.scalatest.FreeSpec

import svm.Util
import scala.Some
import java.util.Arrays
import Gen._
class ArrayTest extends FreeSpec with Util{


  "array stuff" - {
    val tester = new Tester("svm.helloworld.arrays.ArrayStuff")
    "makeIntArray" in chk(tester.run("makeIntArray", _: Int))(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    "makeFloatArray" in tester.run("makeFloatArray")
    "makeStringArray" in tester.run("makeStringArray")
    "arrayLength" in tester.run("arrayLength")
    "arraySet" in tester.run("arraySet")
    "arrayGet" in tester.run("arrayGet")
    "bubbleSort" in chk({ src: Seq[Int] =>
      tester.runC("bubbleSort", Seq(src.toArray))
    })(Seq(
      Seq(0, 1, 2, 3, 4, 5, 6, 7),
      Seq(7, 6, 5, 4, 3, 2, 1, 0),
      Seq(0, 1, 2, 3, 4, 5, 6, 7),
      Seq.fill(10)(util.Random.nextInt()),
      Seq.fill(20)(util.Random.nextInt())
    ))
  }
  "multi dim arrays" - {
    val tester = new Tester("svm.helloworld.arrays.MultiDimArrays")
    "make2D" in chk(tester.run("make2D", _: Int, _: Int))(Seq(0, 1, 2), Seq(0, 1, 2))
    "make3D" in chk(tester.run("make3D", _: Int, _: Int, _ : Int))(Seq(0, 1, 2), Seq(0, 1, 2), Seq(0, 1, 2))
    "getAndSet" in tester.run("getAndSet")
  }

}

