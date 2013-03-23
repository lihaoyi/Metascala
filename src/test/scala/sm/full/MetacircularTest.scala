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
    x.run("fibonacci", 6)
  }

  def innerClass = {
    val x = new sm.Util.SingleClassVM("sm.features.classes.ClassStuff", s => ())
    x.run("innerClass")
  }

  def bubbleSort = {
    val x = new sm.Util.SingleClassVM("sm.features.arrays.ArrayStuff", s => ())
    x.run("makeIntArray", 10)
  }
  def omg = {
    println("Hello Scala!")
  }

}


class MetacircularTest extends FreeSpec with Util{

  val buffer = new BufferLog(4000)
  var count = 0
  val tester = new Tester("sm.full.MetacircularTest")
  "sqrtFinder" in {
    try tester.run("sqrtFinder") catch{case e: UncaughtVmException =>
      buffer.lines.foreach(println)
      e.stackData.foreach(f => println(f.clsName + "/" + f.methodName + " : " + f.lineNumber))
      throw e
    }
  }
  "helloWorld" in {
    tester.run("omg")
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
}

