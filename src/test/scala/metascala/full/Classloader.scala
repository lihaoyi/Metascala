package metascala
package full

import org.scalatest.FreeSpec

import metascala.Util
import scala.Some
import java.util.Arrays
import metascala.Gen._

class ClassTest extends FreeSpec {
  import Util._
  "class stuff" - {
    val tester = new VM()

    "name" in tester.testFunc{ () => new Object().getClass.getName }
    "namePrim" in tester.testFunc{ () => classOf[Int].getName }
    "nameArray" in tester.testFunc{ () => new Array[Object](10).getClass.getName }
    "nameObjArray" in tester.testFunc{ () => new Array[Long](100).getClass.getName }

    "forName" in {
      val func = {(s: String) =>
        val x = Class.forName(s)
        x.getCanonicalName
      }
      chk(tester.testFunc(func) _)(Seq(
        "java.lang.Object",
        "java.lang.Object",
        "java.util.AbstractCollection",
        "[I",
        "[Ljava.lang.Object;"
      ))
    }

    "isPrimitive" in tester.testFunc{ () =>
      Array(
        new Object().getClass.isPrimitive,
        new java.lang.Float(10).getClass.isPrimitive,
        new java.lang.Integer(12).getClass.isPrimitive,
        new java.lang.Boolean(true).getClass.isPrimitive,
        new Array[Int](0).getClass.isPrimitive
      )
    }
    "isArray" in tester.testFunc{ () =>
      Array(
        new Object().getClass.isArray,
        new java.lang.Float(10).getClass.isArray,
        new java.lang.Integer(12).getClass.isArray,
        new java.lang.Boolean(true).getClass.isArray,
        new Array[Int](0).getClass.isArray
      )
    }
  }
  "classloaders" - {
    val tester = new VM()
    "name" in tester.testFunc{ () =>
      val cl = classOf[String].getClassLoader
      "omg" + cl
    }
    //"create" in tester.run("create")
  }
}

