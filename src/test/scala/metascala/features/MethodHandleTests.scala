package metascala.features

import metascala.TestUtil._
import utest._

object MethodHandleTests extends utest.TestSuite {
  def tests = Tests {


    "findStaticField" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Boolean]("findStaticGetter", false)
      tester.run[Boolean]("findStaticGetter", true)
      tester.run[Boolean]("findStaticGetterBoxed", false)
      tester.run[Boolean]("findStaticGetterBoxed", true)

      tester.run[Boolean]("findStaticSetter1", false)
      tester.run[Boolean]("findStaticSetter1", true)
      tester.run[Boolean]("findStaticSetter2", false)
      tester.run[Boolean]("findStaticSetter2", true)
      tester.run[Boolean]("findStaticSetter3", false)
      tester.run[Boolean]("findStaticSetter3", true)
      tester.run[Boolean]("findStaticSetter4", false)
      tester.run[Boolean]("findStaticSetter4", true)
    }

    "findInstanceField" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Long]("findFieldGetter", 31337L)

      tester.run[Long]("findFieldSetter1", 31337L)
      tester.run[Long]("findFieldSetter2", 31337L)
      tester.run[Long]("findFieldSetter3", 31337L)
      tester.run[Long]("findFieldSetter4", 31337L)
    }
    "asType" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Boolean]("asType", false)
      tester.run[Boolean]("asType", true)
    }
    "findStaticMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Double]("findStaticMethod", 31.337)
      tester.run[Double]("findStaticMethod", 3.1415926)
      tester.run[Double]("findStaticMethod", -2.71828182846)
      tester.run[Double]("findStaticMethod", -1337.0)
    }
    "findVirtualMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Char]("findVirtualMethod", 'a')
      tester.run[Char]("findVirtualMethod", 'V')
      tester.run[Char]("findVirtualMethod", 'S')
      tester.run[Char]("findVirtualMethod", '-')
      tester.run[Char]("findVirtualMethod", '1')
    }
    "findSpecialMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Char]("findSpecialMethod", 'a')
      tester.run[Char]("findSpecialMethod", 'B')
      tester.run[Char]("findSpecialMethod", 'c')
      tester.run[Char]("findSpecialMethod", '1')
      tester.run[Char]("findSpecialMethod", '2')
      tester.run[Char]("findSpecialMethod", ' ')
    }
    "findInterfaceMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Char]("findInterfaceMethod", 0)
      tester.run[Char]("findInterfaceMethod", 1)
      tester.run[Char]("findInterfaceMethod", 2)
      tester.run[Char]("findInterfaceMethod", 3)
      tester.run[Char]("findInterfaceMethod", 4)
    }
    "transformStaticMethod" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Double]("transformStaticMethod", 0.5)
      tester.run[Double]("transformStaticMethod", 1.5)
      tester.run[Double]("transformStaticMethod", -100000000.0)
    }
    "varargs" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Boolean]("varargs", true)
    }
    "lambda" - {
      val tester = new Tester("metascala.features.methods.MethodHandles")
      tester.run[Boolean]("lambda", true)
    }
  }
}
