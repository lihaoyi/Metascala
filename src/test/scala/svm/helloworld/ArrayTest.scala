package svm
package helloworld

import org.scalatest.FreeSpec

import svm.Util
import scala.Some
import java.util.Arrays
import Gen._
class ArrayTest extends FreeSpec with Util{

  val tester = new Tester("svm.helloworld.arrays.ArrayStuff")
  "array stuff" - {
    "makeIntArray" in check(tester.run("makeIntArray", _: Int), 10)(Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    "makeFloatArray" in tester.run("makeFloatArray")
    "makeStringArray" in tester.run("makeStringArray")
    "arrayLength" in tester.run("arrayLength")
    "arraySet" in tester.run("arraySet")
    "arrayGet" in tester.run("arrayGet")
    "bubbleSort" in check({ src: Seq[Int] =>
      tester.runC("bubbleSort", Seq(src.toArray))
    }, 5)(Seq(
      Seq(0, 1, 2, 3, 4, 5, 6, 7),
      Seq(7, 6, 5, 4, 3, 2, 1, 0),
      Seq(0, 1, 2, 3, 4, 5, 6, 7),
      Seq.fill(10)(util.Random.nextInt()),
      Seq.fill(20)(util.Random.nextInt())
    ))
  }

}

