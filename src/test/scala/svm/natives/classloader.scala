package svm
package helloworld

import org.scalatest.FreeSpec

import svm.Util
import scala.Some
import java.util.Arrays
import Gen._
class ClassTest extends FreeSpec with Util{
  "class stuff" - {
    val tester = new Tester("svm.natives.classes.ClassObject")
    "name" in tester.run("name")
    "forName" in chk(tester.run("forName", _: String))(Seq("ClassObject", "Object", "Cow"))
    "isPrimitive" in tester.run("isPrimitive")
    "isArray" in tester.run("isArray")
  }

}

