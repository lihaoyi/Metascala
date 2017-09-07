package metascala
package features

import org.scalatest.FreeSpec

import metascala.TestUtil
import scala.Some
import java.util.Arrays
import metascala.Gen._

class GetClassTest extends FreeSpec {
  import TestUtil._
  "class stuff" - {
    val tester = new VM()

    "name" in tester.test{ new Object().getClass.getName }
    "namePrim" in tester.test{ classOf[Int].getName }
    "nameArray" in tester.test{ new Array[Object](10).getClass.getName }
    "nameObjArray" in tester.test{ new Array[Long](100).getClass.getName }

    "forName" in {
      val cases = Array(
        "java.lang.Object",
        "java.lang.Object",
        "java.util.AbstractCollection",
        "[I",
        "[Ljava.lang.Object;"
      )
      for(s <- cases) yield tester.test{
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
    "isAssignableFrom" in tester.test{
      Array(
        new Object().getClass.isAssignableFrom(new String().getClass),
        new String().getClass.isAssignableFrom(new Object().getClass)
      )
    }
    "isInstance" in tester.test{
      Seq(
        classOf[java.lang.Object].isInstance(new java.lang.Object()), // true
        classOf[java.lang.Object].isInstance(new java.lang.Integer(1)), // true
        classOf[java.lang.Object].isInstance(new Array[Int](1)), // true
        classOf[java.lang.Object].isInstance(new Array[java.lang.Object](1)), // true
        classOf[java.lang.Integer].isInstance(new java.lang.Object()), // false
        classOf[java.lang.Integer].isInstance(new Array[java.lang.Object](1)), // false
        classOf[Array[java.lang.Integer]].isInstance(new Array[java.lang.Object](1)), // false
        classOf[Array[java.lang.Object]].isInstance(new Array[java.lang.Integer](1)), // false
        classOf[Array[java.lang.Integer]].isInstance(new Array[java.lang.Integer](1)), // true
        classOf[Array[java.lang.Object]].isInstance(new Array[java.lang.Object](1)), // true
        classOf[Int].isInstance(new java.lang.Integer(1)) // false
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

