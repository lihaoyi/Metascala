package sm.full

import sm.{UncaughtVmException, Gen, Util}
import org.scalatest.FreeSpec
import sm.Util.{SingleClassVM}
import com.fasterxml.jackson.databind.ObjectMapper

object Moo{
  def sqrtFinder() = {
    val x = new sm.Util.SingleClassVM("sm.features.controlflow.Loops", s => ())
    x.run("sqrtFinder", 5.0)
  }
  def doubleSqrtFinder() = {
    val x = new sm.Util.SingleClassVM("sm.full.Moo", s => ())
    x.run("run")
  }
  def helloWorld = {
    val x = new sm.Util.SingleClassVM("sm.features.methods.Statics", s => ())
    x.run("helloWorld", 1)
  }
  def runJson() = {
    val json = """
      |{
      |    "firstName": "John",
      |    "lastName": "Smith",
      |    "age": 25,
      |    "address": {
      |        "streetAddress": "21 2nd Street",
      |        "city": "New York",
      |        "state": "NY",
      |        "postalCode": 10021
      |    },
      |    "phoneNumber": [
      |        {
      |            "type": "home",
      |            "number": "212 555-1234"
      |        },
      |        {
      |            "type": "fax",
      |            "number": "646 555-4567"
      |        }
      |    ]
      |}
    """.stripMargin
    val mapper = new ObjectMapper()
    val root = mapper.readTree(json)
    root.get("age").asInt()
  }
}

class SVMTest extends FreeSpec with Util{
  "sqrtFinder" in {
    val tester = new Tester("sm.full.Moo")
    tester.run("sqrtFinder")
  }
  "helloWorld" in {
    val tester = new Tester("sm.full.Moo")
    try{
      tester.run("helloWorld")
    }catch{ case endWith @ UncaughtVmException(name, msg, stackTrace, dump) =>
      println(name)
      println(msg)
      dump.reverse.foreach{s =>
        println(s.clsName + "/"+s.methodName)
        s.bytecodes.foreach(x =>
          println("\t" + x)
        )
      }


    }
  }
  "doubleSqrtFinder" in {
    val tester = new Tester("sm.full.Moo")
    try{
      tester.run("doubleSqrtFinder")
    }catch{ case endWith @ UncaughtVmException(name, msg, stackTrace, dump) =>
      println(name)
      println(msg)
      dump.reverse.foreach{s =>
        println(s.clsName + "/"+s.methodName)
        s.bytecodes.foreach(x =>
          println("\t" + x)
        )
      }
    }
  }
  /*"rhino" in {
    val tester = new Tester("sm.full.SVM")
    tester.run("rhino")
  }*/
  /*"json" in {

    val tester = new Tester("sm.full.Moo")
    try
      tester.run("runJson")
    catch{ case e: UncaughtVmException =>
      println(e.msg)
      e.stackData.map(f => f.clsName + f.methodName).foreach(println)
    }
  }*/
}

