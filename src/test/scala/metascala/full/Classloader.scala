package metascala.full

import org.scalatest.FreeSpec

import metascala.Util
import scala.Some
import java.util.Arrays
import metascala.Gen._

class ClassTest extends FreeSpec with Util{
  "class stuff" - {
    val tester = new Tester("metascala.full.ClassObject", x => println(x))

    "name" in tester.run("name")
    "namePrim" in tester.run("namePrim")
    "nameArray" in tester.run("nameArray")
    "nameObjArray" in tester.run("nameObjArray")

    "forName" in {
      chk(tester.run("forName", _: String))(Seq(
        "metascala.full.ClassObject",
        "java.lang.Object",
        "java.util.AbstractCollection",
        "[I",
        "[Ljava.lang.Object;"
      ))
    }

    "isPrimitive" in tester.run("isPrimitive")
    "isArray" in {
      tester.run("isArray")
    }
  }
  "classloaders" - {
    val tester = new Tester("metascala.full.ClassLoaders")
    "name" in tester.run("name")
    //"create" in tester.run("create")
  }
}

