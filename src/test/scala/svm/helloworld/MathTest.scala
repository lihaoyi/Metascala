package svm
package helloworld


import org.scalatest.FreeSpec
import Gen._
import svm.Util
import util.Random



class MathTest extends FreeSpec with Util{


  "single precision" - {
    val tester = new Tester("svm.helloworld.math.HelloMath")
    "hello math" - {

      "imain" in tester.run("imain")
      "fmain" in tester.run("fmain")
      "amain" in tester.run("amain")
    }
    "basic math" - {
      "int" - {
        "ineg" in check(tester.run("ineg", _: Int), 10)
        "iadd" in check(tester.run("iadd", _: Int, _: Int), 10)
        "isub" in check(tester.run("isub", _: Int, _: Int), 10)
        "imul" in check(tester.run("imul", _: Int, _: Int), 10)
        "idiv" in check(tester.run("idiv", _: Int, _: Int), 10)
        "imod" in check(tester.run("imod", _: Int, _: Int), 10)
      }
      "float" - {
        "fneg" in check(tester.run("fneg", _: Float), 10)
        "fadd" in check(tester.run("fadd", _: Float, _: Float), 10)
        "fsub" in check(tester.run("fsub", _: Float, _: Float), 10)
        "fmul" in check(tester.run("fmul", _: Float, _: Float), 10)
        "fdiv" in check(tester.run("fdiv", _: Float, _: Float), 10)
        "fmod" in check(tester.run("fmod", _: Float, _: Float), 10)
      }
      "more int stuff" - {
        "ishl" in check(tester.run("ishl", _: Int, _: Int), 10)(Gen.intAll, Gen.int(5))
        "ishr" in check(tester.run("ishr", _: Int, _: Int), 10)(Gen.intAll, Gen.int(5))
        "iushr" in check(tester.run("iushr", _: Int, _: Int), 10)(Gen.intAll, Gen.int(5))
        "iand" in check(tester.run("iand", _: Int, _: Int), 10)
        "ior" in check(tester.run("ior", _: Int, _: Int), 10)
        "ixor" in check(tester.run("ixor", _: Int, _: Int), 10)
      }

    }
  }
  "double precision" - {
    val tester = new Tester("svm.helloworld.math.HelloLongs")
    "hello longs" - {

      "lmain" in tester.run("lmain")
      "dmain" in tester.run("dmain")
    }
    "basic math" - {
      "long" - {
        "lneg" in check(tester.run("lneg", _: Long), 10)
        "ladd" in check(tester.run("ladd", _: Long, _: Long), 10)
        "lsub" in check(tester.run("lsub", _: Long, _: Long), 10)
        "lmul" in check(tester.run("lmul", _: Long, _: Long), 10)
        "ldiv" in check(tester.run("ldiv", _: Long, _: Long), 10)
        "lmod" in check(tester.run("lmod", _: Long, _: Long), 10)
      }
      "double" - {
        "dneg" in check(tester.run("dneg", _: Double), 10)
        "dadd" in check(tester.run("dadd", _: Double, _: Double), 10)
        "dsub" in check(tester.run("dsub", _: Double, _: Double), 10)
        "dmul" in check(tester.run("dmul", _: Double, _: Double), 10)
        "ddiv" in check(tester.run("ddiv", _: Double, _: Double), 10)
        "dmod" in check(tester.run("dmod", _: Double, _: Double), 10)
      }

      "more long stuff" - {
        "lshl" in check(tester.run("lshl", _: Long, _: Long), 10)(Gen.longAll, Gen.long(6))
        "lshr" in check(tester.run("lshr", _: Long, _: Long), 10)(Gen.longAll, Gen.long(6))
        "lushr" in check(tester.run("lushr", _: Long, _: Long), 10)(Gen.longAll, Gen.long(6))
        "land" in check(tester.run("land", _: Long, _: Long), 10)
        "lor" in check(tester.run("lor", _: Long, _: Long), 10)
        "lxor" in check(tester.run("lxor", _: Long, _: Long), 10)
      }
    }
  }





}

