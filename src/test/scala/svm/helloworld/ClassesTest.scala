package svm.helloworld

import org.scalatest.FreeSpec
import svm.Util

/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 2/26/13
 * Time: 7:35 PM
 * To change this template use File | Settings | File Templates.
 */
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
  }
}
