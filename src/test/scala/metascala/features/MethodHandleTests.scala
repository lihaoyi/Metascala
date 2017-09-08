package metascala.features

import metascala.TestUtil._
import utest._

object MethodHandleTests extends utest.TestSuite {
  def tests = this {


    "findStaticField" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
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

    "findInstanceField" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run("findFieldGetter", 31337L)

      tester.run("findFieldSetter1", 31337L)
      tester.run("findFieldSetter2", 31337L)
      tester.run("findFieldSetter3", 31337L)
      tester.run("findFieldSetter4", 31337L)
    }
    "asType" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run("asType", false)
      tester.run("asType", true)
    }
    "findStaticMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run("findStaticMethod", 31.337)
      tester.run("findStaticMethod", 3.1415926)
      tester.run("findStaticMethod", -2.71828182846)
      tester.run("findStaticMethod", -1337.0)
    }
    "findVirtualMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run("findVirtualMethod", 'a')
      tester.run("findVirtualMethod", 'V')
      tester.run("findVirtualMethod", 'S')
      tester.run("findVirtualMethod", '-')
      tester.run("findVirtualMethod", '1')
    }
    "findSpecialMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run("findSpecialMethod", 'a')
      tester.run("findSpecialMethod", 'B')
      tester.run("findSpecialMethod", 'c')
      tester.run("findSpecialMethod", '1')
      tester.run("findSpecialMethod", '2')
      tester.run("findSpecialMethod", ' ')
    }
    "findInterfaceMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run("findInterfaceMethod", 0)
      tester.run("findInterfaceMethod", 1)
      tester.run("findInterfaceMethod", 2)
      tester.run("findInterfaceMethod", 3)
      tester.run("findInterfaceMethod", 4)
    }

  }
}
