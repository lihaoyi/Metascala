package svm.helloworld

import org.scalatest.FreeSpec
import svm.Util

class ClassesTest extends FreeSpec with Util{

  "classes" - {
    val tester = new Tester("svm.helloworld.classes.ClassStuff")
    "customClass" in tester.run("customClass")
    "inheritence" in tester.run("inheritence")
    "constructor" in tester.run("constructor")
    "superConstructor" in tester.run("superConstructor")
    "override" in tester.run("override")
    "innerClass" in tester.run("innerClass")
  }
  "interfaces" - {
    val tester = new Tester("svm.helloworld.classes.Interfaces")
    "implement" in tester.run("implement", 10)
    "abstractClass" in tester.run("abstractClass")
    "shadowedInherited" in tester.run("shadowedInherited")
  }
}
