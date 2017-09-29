package metascala
package core

import utest._

import scala.util.{Failure, Try}
import metascala.util.UncaughtVmException

object IOTest extends utest.TestSuite {

  import TestUtil._

  def tests = Tests {
    val tester = new VM()
    "primitives" - {
      "retInt" - tester.testSafe(1337)
      "retDouble" - tester.testSafe(3.1337)
      "argInt" - tester.testFunc((i: Int) => i)(10)
      "argDouble" - tester.testFunc((i: Double) => i)(10.01)
      "multiArgD" - tester.testFunc((i: Int, d: Double) => d)(27, 3.14)
      "multiArgI" - tester.testFunc((i: Int, d: Double) => i)(27, 3.14)

      "stringLiteral" - tester.testFunc(() => "omgwtfbbq")
      "nullStringLiteral" - tester.testFunc(() => null: String)
      "strings" - tester.testFunc((s: String) => s + "a")("mooo")
      "nullReturn" - tester.testFunc(() => null)
      "arrayObj" - tester.testFunc{ () =>
        val arr = new Array[Int](3)
        arr(0) = 1
        arr(1) = 2
        arr(2) = 4
        arr
      }
      "longArrayObj" - tester.testFunc{ () =>
        val arr = new Array[Long](3)
        arr(0) = Long.MinValue
        arr(1) = Long.MaxValue
        arr(2) = 1234567890L
        arr
      }

    }
    "exceptions" - {
      "simple" - {
        val UncaughtVmException(wrapped) = intercept[UncaughtVmException]{
          tester.test{
            simpleException()
          }
        }

        assert(
          wrapped.clsName == "java.lang.NullPointerException",
          wrapped.msg == "null",
          wrapped.cause == null,
          wrapped.getStackTrace.head.getClassName == "metascala.core.IOTest$",
          wrapped.getStackTrace.head.getMethodName == "simpleException"
        )
        wrapped.getStackTrace.mkString("\n")
      }
      "chained" - {
        val UncaughtVmException(wrapped) = intercept[UncaughtVmException]{
          tester.test{
            chainedException()
          }
        }

        assert(
          wrapped.clsName == "java.lang.Exception",
          wrapped.msg == "Wrapper2!",
          // get third element, to skip first two stack frames inside exception constructor
          wrapped.getStackTrace.apply(2).getClassName == "metascala.core.IOTest$",
          wrapped.getStackTrace.apply(2).getMethodName == "chainedException",
          wrapped.cause.clsName == "java.lang.Exception",
          wrapped.cause.msg == "Wrapper!",
          wrapped.cause.getStackTrace.apply(2).getClassName == "metascala.core.IOTest$",
          wrapped.cause.getStackTrace.apply(2).getMethodName == "chainedException0",
          wrapped.cause.cause.clsName == "java.lang.NullPointerException",
          wrapped.cause.cause.msg == "null",
          wrapped.cause.cause.getStackTrace.head.getClassName == "metascala.core.IOTest$",
          wrapped.cause.cause.getStackTrace.head.getMethodName == "simpleException"
        )
      }
    }
  }
  def simpleException() = {
    val s: String = null
    s.charAt(0)
    10
  }
  def chainedException0() = {
    try{
      simpleException()
    }catch{ case e: Throwable =>
      throw new Exception("Wrapper!", e)
    }
  }
  def chainedException() = {
    try{
      chainedException0()
    }catch{ case e: Throwable =>
      throw new Exception("Wrapper2!", e)
    }
  }
}