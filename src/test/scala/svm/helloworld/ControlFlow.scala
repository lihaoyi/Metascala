package svm.helloworld


import org.scalatest.FreeSpec
import svm.Util._
import svm.Util
import scala.Some

class ControlFlow extends FreeSpec{
  import Util.loadClass

  "if else" - {
    val vm = new SingleClassVirtualMachine("svm.helloworld.controlflow.IfElse", loadClass)
    "basicIf" in assert(vm.run("basicIf") === 10)
    "ifElseIf" in assert(vm.run("ifElseIf") === 312)
    "ifElseIfBig" in assert(vm.run("ifElseIfBig") === 5)
  }
  "loops" - {
    val vm = new SingleClassVirtualMachine("svm.helloworld.controlflow.Loops", loadClass)
    "nullFor" in assert(vm.run("nullFor") === 0)
    "basicFor" in assert(vm.run("basicFor") === 1024)
    "nullWhile" in assert(vm.run("nullWhile") === 1)
    "basicWhile" in assert(vm.run("basicWhile") === 1024)
  }


}

