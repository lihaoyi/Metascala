package svm

import org.scalatest.FreeSpec
import svm.Util.SingleClassVirtualMachine

class ControlFlow extends FreeSpec{
  import Util.loadClass

  "control flow" in {
    val vm = new SingleClassVirtualMachine("helloworld.ControlFlow", loadClass)
    assert(vm.run("basicIf") === Some(10))
    assert(vm.run("ifElseIf") === Some(312))
    assert(vm.run("ifElseIfBig") === Some(5))
  }


}
