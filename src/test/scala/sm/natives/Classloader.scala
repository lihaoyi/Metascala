package sm
package features

import org.scalatest.FreeSpec

import sm.Util
import scala.Some
import java.util.Arrays
import Gen._
class ClassTest extends FreeSpec with Util{
  "class stuff" - {
    val tester = new Tester("sm.natives.classes.ClassObject")
    "name" in tester.run("name")
    "forName" in chk(tester.run("forName", _: String))(Seq("sm.natives.classes.ClassObject", "java.lang.Object", "java.util.AbstractCollection"))
    "isPrimitive" in tester.run("isPrimitive")
    "isArray" in tester.run("isArray")
  }
  "classloaders" - {
    val tester = new Tester("sm.natives.classes.ClassLoaders")
    "name" in tester.run("name")
    //"create" in tester.run("create")
  }
}

