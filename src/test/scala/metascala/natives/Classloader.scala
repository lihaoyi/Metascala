package metascala
package features

import org.scalatest.FreeSpec

import metascala.Util
import scala.Some
import java.util.Arrays
import Gen._

class ClassTest extends FreeSpec with Util{
  "class stuff" - {
    val tester = new Tester("metascala.natives.classes.ClassObject", x => println(x))

    "name" in tester.run("name")
    "namePrim" in tester.run("namePrim")
    "nameArray" in tester.run("nameArray")
    "nameObjArray" in tester.run("nameObjArray")

    "forName" in {
      chk(tester.run("forName", _: String))(Seq("metascala.natives.classes.ClassObject", "java.lang.Object", "java.util.AbstractCollection"))
    }

    "isPrimitive" in tester.run("isPrimitive")
    "isArray" in {
      tester.run("isArray")
    }
  }
  "classloaders" - {
    val tester = new Tester("metascala.natives.classes.ClassLoaders")
    "name" in tester.run("name")
    //"create" in tester.run("create")
  }
}

