package metascala
package features

import org.scalatest.FreeSpec

import metascala.Util
import scala.Some
import java.util.Arrays
import Gen._
class ClassTest extends FreeSpec with Util{
  "class stuff" - {
    val buffer = new BufferLog(4000)
    val tester = new Tester("metascala.natives.classes.ClassObject", buffer)
    "name" in tester.run("name")
    "forName" in {
      try{
        chk(tester.run("forName", _: String))(Seq("metascala.natives.classes.ClassObject", "java.lang.Object", "java.util.AbstractCollection"))
      }catch { case e =>
        buffer.lines.foreach(println)
        throw e
      }
    }
    "isPrimitive" in tester.run("isPrimitive")
    "isArray" in {try{
      tester.run("isArray")
    }catch { case e =>
    buffer.lines.foreach(println)
    throw e
    }}
  }
  "classloaders" - {
    val tester = new Tester("metascala.natives.classes.ClassLoaders")
    "name" in tester.run("name")
    //"create" in tester.run("create")
  }
}

