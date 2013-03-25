package sm
package full

import sm.{UncaughtVmException, Gen, Util}
import org.scalatest.FreeSpec
import sm.Util.{SingleClassVM}
import com.fasterxml.jackson.databind.ObjectMapper
import collection.GenSeq

object MetacircularTest{
  def sqrtFinder = {
    val x = new sm.Util.SingleClassVM("sm.features.controlflow.Loops", s => ())
    x.run("sqrtFinder", 5.0)
  }

  def fibonacci = {
    val x = new sm.Util.SingleClassVM("sm.features.methods.Statics", s => ())
    x.run("fibonacci", 12)
  }

  def innerClass = {
    val x = new sm.Util.SingleClassVM("sm.features.classes.ClassStuff", s => ())
    x.run("innerClass")
  }

  def bubbleSort = {
    val x = new sm.Util.SingleClassVM("sm.features.arrays.ArrayStuff", s => ())
    x.run("bubbleSort", Array(6, 5, 2, 7, 3, 4, 9, 1, 8)).cast[Array[Int]]
  }
  def getAndSet = {
    val x = new sm.Util.SingleClassVM("sm.features.arrays.MultiDimArrays", s => ())
    x.run("getAndSet")
  }
  def multiCatch = {
    val x = new sm.Util.SingleClassVM("sm.features.exceptions.Exceptions", s => ())
    x.run("multiCatch", 2)
  }
  def helloWorld = {
    println("Hello Scala!")
  }

}


class MetacircularTest extends FreeSpec with Util{

  val buffer = new BufferLog(4000)
  var count = 0

  val tester = new Tester("sm.full.MetacircularTest")
  "sqrtFinder" in {
    tester.run("sqrtFinder")
  }
  "helloWorld" in {
    tester.run("helloWorld")
  }

  "fibonacci" in {
    tester.run("fibonacci")
    println(tester.svm.threads(0).getI)
  }


  "innerClass" in {
    tester.run("innerClass")
  }
  "bubbleSort" in {
    tester.run("bubbleSort")

  }
  "getAndSet" in {
    tester.run("getAndSet")
    println(tester.svm.threads(0).getI)
  }

  "multiCatch" in {
    tester.run("multiCatch")
    println(tester.svm.threads(0).getI)
  }
}

