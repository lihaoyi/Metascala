package svm.helloworld

import org.scalatest.FreeSpec
import svm.Util._
import svm.Util
import scala.Some

class ArrayTest extends FreeSpec{
  import Util.loadClass

  "array stuff" - {
    val vm = new SingleClassVirtualMachine("svm.helloworld.arrays.ArrayStuff", loadClass)
    "makeIntArray" in { val Array(0, 0, 0, 0, 0) = vm.run("makeIntArray") }
    "makeFloatArray" in { val Array(0.25f, 0.5f, 0.75f) = vm.run("makeFloatArray") }
    "makeStringArray" in { val Array("omg", "wtf", "bbq") = vm.run("makeStringArray") }
    "arrayLength" in assert(vm.run("arrayLength") === 9)
    "arraySet" in { val Array(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) = vm.run("arraySet") }
    "arrayGet" in assert(vm.run("arrayGet") === 4545)
  }

}

