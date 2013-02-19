package svm.helloworld

import org.scalatest.FreeSpec
import svm.Util._
import svm.Util
import scala.Some

class ControlFlow extends FreeSpec{
  import Util.loadClass

  "if else" - {
    val vm = new SingleClassVirtualMachine("helloworld.controlflow.IfElse", loadClass)
    "basicIf" in assert(vm.run("basicIf") === Some(10))
    "ifElseIf" in assert(vm.run("ifElseIf") === Some(312))
    "ifElseIfBig" in assert(vm.run("ifElseIfBig") === Some(5))
  }
  "loops" - {
    val vm = new SingleClassVirtualMachine("helloworld.controlflow.Loops", loadClass)
    "nullFor" in assert(vm.run("nullFor") === Some(0))
    "basicFor" in assert(vm.run("basicFor") === Some(1024))
    "nullWhile" in assert(vm.run("nullWhile") === Some(1))
    "basicWhile" in assert(vm.run("basicWhile") === Some(1024))
  }


}
