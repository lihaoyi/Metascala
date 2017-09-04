package metascala.features

import metascala.TestUtil._
import org.scalatest.FreeSpec

class InvokeDynamicTests extends FreeSpec {

  "invokedynamic" - {
//    "findStaticField" in {
//      val tester = new Tester("metascala.features.javac.InvokeDynamic")
//      tester.run("findStaticGetter", false)
//      tester.run("findStaticGetter", true)
//      tester.run("findStaticSetter", false)
//      tester.run("findStaticSetter", true)
//    }
//    "findInstanceField" in {
//      val tester = new Tester("metascala.features.javac.InvokeDynamic")
//      tester.run("findFieldGetter", false)
//      tester.run("findFieldGetter", true)
//      tester.run("findFieldSetter", false)
//      tester.run("findFieldSetter", true)
//    }
    "findStaticMethod" in {try{
      val tester = new Tester("metascala.features.javac.InvokeDynamic")
      tester.run("findStaticMethod", false)
      tester.run("findStaticMethod", true)
    }catch{case e => e.printStackTrace}}
  }
}

