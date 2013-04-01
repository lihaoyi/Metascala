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

  val buffer = new BufferLog(4000)
  var count = 0

  val tester = new Tester("sm.full.MetacircularTest")
  "helloWorld" in {
    tester.run("helloWorld")
  }
  "sqrtFinder" in {
    try{
    tester.run("sqrtFinder")
    println(tester.svm.threads(0).getI)
    }catch {case e =>
      buffer.lines.foreach(println)
      throw e
    }

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

  /*"doubleMetaOne" in {
    tester.run("doubleMetaOne")
    println(tester.svm.threads(0).getI)
  }*/
}

