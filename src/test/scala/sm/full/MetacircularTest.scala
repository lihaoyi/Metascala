package sm
package full

import sm.{UncaughtVmException, Gen, Util}
import org.scalatest.FreeSpec
import sm.Util.{SingleClassVM}
import com.fasterxml.jackson.databind.ObjectMapper
import collection.GenSeq

object MetacircularTest{
  def sqrtFinder() = {
    val x = new sm.Util.SingleClassVM("sm.features.controlflow.Loops", s => ())
    x.run("sqrtFinder", 5.0)
  }

  def helloWorld = {
    val x = new sm.Util.SingleClassVM("sm.features.methods.Statics", s => ())
    x.run("helloWorld", 1)
  }
  def fibonacci = {
    val x = new sm.Util.SingleClassVM("sm.features.methods.Statics", s => ())
    x.run("fibonacci", 6)
  }

  def getAndSet = {

  }

}

class MetacircularTest extends FreeSpec with Util{
  val tester = new Tester("sm.full.MetacircularTest")
  "sqrtFinder" in {
    tester.run("sqrtFinder")
  }
  "helloWorld" in {
    tester.run("helloWorld")
  }
  "fibonacci" in {
    tester.run("fibonacci")
  }



}

