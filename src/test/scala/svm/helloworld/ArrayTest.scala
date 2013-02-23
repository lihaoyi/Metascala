package svm.helloworld

import org.scalatest.FreeSpec

import svm.Util
import scala.Some
import java.util.Arrays

class ArrayTest extends FreeSpec with Util{

  val tester = new Tester("svm.helloworld.arrays.ArrayStuff")
  "array stuff" - {
    "makeIntArray" in tester.run("makeIntArray")
    "makeFloatArray" in tester.run("makeFloatArray")
    "makeStringArray" in tester.run("makeStringArray")
    "arrayLength" in tester.run("arrayLength")
    "arraySet" in tester.run("arraySet")
    "arrayGet" in tester.run("arrayGet")
    "bubbleSort" in {
      val src = Seq.fill(10)(util.Random.nextInt())
      tester.runC("bubbleSort", Seq(src.toArray))
    }
  }

}

