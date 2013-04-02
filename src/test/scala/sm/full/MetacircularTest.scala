package sm
package full

import sm.{UncaughtVmException, Gen, Util}
import org.scalatest.FreeSpec
import sm.Util.{SingleClassVM}
import com.fasterxml.jackson.databind.ObjectMapper
import collection.GenSeq

object MetacircularTest{
  def sqrtFinder = {
    val x = new sm.VM()
    x.invoke("sm.features.controlflow.Loops", "sqrtFinder", Seq(5.0))
  }

  def fibonacci = {
    val x = new sm.VM()
    x.invoke("sm.features.methods.Statics", "fibonacci", Seq(12))
  }

  def innerClass = {
    val x = new sm.VM()
    x.invoke("sm.features.classes.ClassStuff", "innerClass")
  }

  def bubbleSort = {
    val x = new sm.VM()
    x.invoke("sm.features.arrays.ArrayStuff", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
  }
  def getAndSet = {
    val x = new sm.VM()
    x.invoke("sm.features.arrays.MultiDimArrays", "getAndSet")
  }
  def multiCatch = {
    val x = new sm.VM()
    x.invoke("sm.features.exceptions.Exceptions", "multiCatch", Seq(2))
  }

  def doubleMetaOne = {
    val x = new sm.VM()
    x.invoke("sm.full.MetacircularTest", "doubleMetaTwo")
  }
  def doubleMetaTwo = {
    val x = new sm.VM()
    x.invoke("sm.full.MetacircularTest", "helloWorld")
  }
  def helloWorld = {
    println("Hello Scala!")
  }
}

class MetacircularTest extends FreeSpec with Util{

  val buffer = new BufferLog(8000)
  var count = 0

  val tester = new Tester("sm.full.MetacircularTest")
  "helloWorld" in {
    tester.run("helloWorld")
  }
  "sqrtFinder" in {
    tester.run("sqrtFinder")
  }

  "fibonacci" in {
    tester.run("fibonacci")
  }


  "innerClass" in {
    tester.run("innerClass")
  }
  "bubbleSort" in {
    tester.run("bubbleSort")

  }
  "getAndSet" in {
    tester.run("getAndSet")
    println(tester.svm.threads(0).getOpCount)
  }

  "multiCatch" in {
    tester.run("multiCatch")
    println(tester.svm.threads(0).getOpCount)
  }
  /*"doubleMetaOne" in {
    tester.run("doubleMetaOne")
    println(tester.svm.threads(0).getOpCount)
  }*/
}

