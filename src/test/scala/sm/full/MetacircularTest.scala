package sm
package full

import sm.{UncaughtVmException, Gen, Util}
import org.scalatest.FreeSpec
import sm.Util.{SingleClassVM}
import com.fasterxml.jackson.databind.ObjectMapper
import collection.GenSeq

object MetacircularTest{
  /*def sqrtFinder = {
    val x = new sm.Util.SingleClassVM("sm.features.controlflow.Loops", s => ())
    x.run("sqrtFinder", 5.0)
  }

  def helloWorld = {
    println("Hello World Starts!")
    /*val x = new sm.Util.SingleClassVM("sm.features.methods.Statics", x => println(x))
    println("VM Initialized!!")
    x.run("helloWorld", 1)*/
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
  }*/
  def omg = {
    println("OMG")
  }

}


class MetacircularTest extends FreeSpec with Util{

  val buffer = new BufferLog(4000)
  var count = 0
  val tester = new Tester("sm.full.MetacircularTest", buffer)
  /*"sqrtFinder" in {
    tester.run("sqrtFinder")
  }*/
  "helloWorld" in {try
    tester.run("omg")
    catch{case e =>
      buffer.lines.foreach(println)
      throw e
    }
  }

  /*
  "fibonacci" in {
    tester.run("fibonacci")
  }


  "innerClass" in {
    tester.run("innerClass")
  }*/
  /*"bubbleSort" in {
    try{
      tester.run("bubbleSort")
    }catch { case UncaughtVmException(name, msg, st, sd) =>
      println(name + ": " + msg)
      sd.foreach { f =>
        println(f.clsName + "/" + f.methodName + " " + f.fileName + ":" + f.lineNumber)
      }
      for(i <- 0 until n){
        println(buffer((i + index) % n))
      }

    }
  }*/
}

