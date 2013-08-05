package metascala
package full

//import metascala.{UncaughtVmException, Gen, Util}
import org.scalatest.FreeSpec
import metascala.Util.{SingleClassVM}

import collection.GenSeq

object MetacircularTest{

  def sqrtFinder = {
    val x = new metascala.VM(memorySize = 1024)
    x.invoke("metascala/features/controlflow/Loops", "sqrtFinder", Seq(5.0))
  }

  def fibonacci = {
    val x = new metascala.VM()
    x.invoke("metascala/features/methods/Statics", "fibonacci", Seq(12))
  }

  def innerClass = {
    val x = new metascala.VM()
    x.invoke("metascala/features/classes/ClassStuff", "innerClass")
  }

  def bubbleSort = {
    val x = new metascala.VM()
    x.invoke("metascala/features/arrays/ArrayStuff", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
  }
  def getAndSet = {
    val x = new metascala.VM()
    x.invoke("metascala.features.arrays.MultiDimArrays", "getAndSet")
  }
  def multiCatch = {
    val x = new metascala.VM()
    x.invoke("metascala.features.exceptions.Exceptions", "multiCatch", Seq(2))
  }

  def doubleMetaOne = {
    val x = new metascala.VM()
    x.invoke("metascala.full.MetacircularTest", "doubleMetaTwo")
  }
  def doubleMetaTwo = {
    val x = new metascala.VM()
    x.invoke("metascala.features.controlflow.Loops", "sqrtFinder", Seq(5.0))

  }
  def helloWorld = {
    println("Hello Scala!")
  }
}

class MetacircularTest extends FreeSpec with Util{

  val buffer = new BufferLog(1900)
  var count = 0



//  "helloWorld" in {
//    tester.run("helloWorld")
//  }

  "sqrtFinder" in {
    val tester = new Tester("metascala.full.MetacircularTest", memorySize = 3 * 1024 * 1024)
    for(i <- 0 to 5) tester.run("sqrtFinder")

  }

  "fibonacci" in {
    val tester = new Tester("metascala.full.MetacircularTest", memorySize = 3 * 1024 * 1024)
    for(i <- 0 to 5) tester.run("fibonacci")
  }

  "innerClass" in {
    val tester = new Tester("metascala.full.MetacircularTest", memorySize = 3 * 1014 * 1024)
    for(i <- 0 to 5) tester.run("innerClass")
  }

  "bubbleSort" in {
    val tester = new Tester("metascala.full.MetacircularTest", memorySize = 5 * 1014 * 1024)
    tester.run("bubbleSort")

  }
  /*
  "getAndSet" in {
    tester.run("getAndSet")
    println(tester.svm.threads(0).getOpCount)
  }

  "multiCatch" in {
    tester.run("multiCatch")
    println(tester.svm.threads(0).getOpCount)
  }*/
  /*"doubleMetaOne" in {
    tester.run("doubleMetaOne")
    println(tester.svm.threads(0).getOpCount)
  }*/
}

