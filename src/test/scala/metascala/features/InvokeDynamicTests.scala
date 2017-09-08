package metascala.features

import metascala.TestUtil._
import org.scalatest.FreeSpec

class InvokeDynamicTests extends FreeSpec {

  "invokedynamic" - {
    "findStaticField" in {
      val tester = new Tester("metascala.features.javac.InvokeDynamic")
      tester.run("findStaticGetter", false)
      tester.run("findStaticGetter", true)
      tester.run("findStaticGetterBoxed", false)
      tester.run("findStaticGetterBoxed", true)

      tester.run("findStaticSetter1", false)
      tester.run("findStaticSetter1", true)
      tester.run("findStaticSetter2", false)
      tester.run("findStaticSetter2", true)
      tester.run("findStaticSetter3", false)
      tester.run("findStaticSetter3", true)
      tester.run("findStaticSetter4", false)
      tester.run("findStaticSetter4", true)
    }

    "findInstanceField" in {
      val tester = new Tester("metascala.features.javac.InvokeDynamic")
      tester.run("findFieldGetter", 31337L)

      tester.run("findFieldSetter1", 31337L)
      tester.run("findFieldSetter2", 31337L)
      tester.run("findFieldSetter3", 31337L)
      tester.run("findFieldSetter4", 31337L)
    }
    "asType" in {
      val tester = new Tester("metascala.features.javac.InvokeDynamic")
      tester.run("asType", false)
      tester.run("asType", true)
    }
    "findStaticMethod" in {
      val tester = new Tester("metascala.features.javac.InvokeDynamic")
      tester.run("findStaticMethod", 31.337)
      tester.run("findStaticMethod", 3.1415926)
      tester.run("findStaticMethod", -2.71828182846)
      tester.run("findStaticMethod", -1337.0)
    }
  }
}

