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

    "name" in tester.test{ new Object().getClass.getName }
    "namePrim" in tester.test{ classOf[Int].getName }
    "nameArray" in tester.test{ new Array[Object](10).getClass.getName }
    "nameObjArray" in tester.test{ new Array[Long](100).getClass.getName }

    "forName" in {
      val cases = Seq(
        "java.lang.Object",
        "java.lang.Object",
        "java.util.AbstractCollection",
        "[I",
        "[Ljava.lang.Object;"
      )
      for(s <- cases) tester.test{
        val x = Class.forName(s)
        x.getCanonicalName
      }
    }
    "forNameBad" in tester.test{
      try{
        val cls = Class.forName("lol")
        cls.getName
      }catch {case x: ClassNotFoundException =>
        x.getMessage
      }
    }

    "isPrimitive" in tester.test{
      Array(
        new Object().getClass.isPrimitive,
        new java.lang.Float(10).getClass.isPrimitive,
        new java.lang.Integer(12).getClass.isPrimitive,
        new java.lang.Boolean(true).getClass.isPrimitive,
        new Array[Int](0).getClass.isPrimitive
      )
    }
    "isArray" in tester.test{
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
    "name" in tester.test{
      val cl = classOf[String].getClassLoader
      "omg" + cl
    }
    //"create" in tester.run("create")
  }
}

