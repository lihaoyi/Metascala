package svm

import model.Util
import org.scalatest.FreeSpec
import java.io.DataInputStream

class VirtualMachineTest extends FreeSpec{
  def loadClass(name: String) = {
    val stream = new DataInputStream(
      getClass.getResourceAsStream(s"/${name.replace('.', '/')}.class")
    )
    val bytes = new Array[Byte](stream.available())
    stream.readFully(bytes)
    bytes
  }

  "hello world" in {
    val vm = new VirtualMachine(loadClass)


    vm.classes("helloworld.HelloMath")
    assert(vm.run("helloworld.HelloMath", "imain") === Some(1337))
    assert(vm.run("helloworld.HelloMath", "fmain") === Some(1.337f))
    assert(vm.run("helloworld.HelloMath", "lmain") === Some(31337))
    assert(vm.run("helloworld.HelloMath", "dmain") === Some(31.337))
    assert(vm.run("helloworld.HelloMath", "amain") === Some("i am 3l33t"))
  }
  "basic math" - {
    "int" in {
      val vm = new VirtualMachine(loadClass)
      vm.classes("helloworld.HelloMath")
      assert(vm.run("helloworld.HelloMath", "iadd") === Some(17))
      assert(vm.run("helloworld.HelloMath", "isub") === Some(9))
      assert(vm.run("helloworld.HelloMath", "imul") === Some(52))
      assert(vm.run("helloworld.HelloMath", "idiv") === Some(3))
      assert(vm.run("helloworld.HelloMath", "imod") === Some(1))
    }
    "long" in {
      val vm = new VirtualMachine(loadClass)
      vm.classes("helloworld.HelloMath")
      println(vm.run("helloworld.HelloMath", "ladd"))
      println(vm.run("helloworld.HelloMath", "lsub"))
      println(vm.run("helloworld.HelloMath", "lmul"))
      println(vm.run("helloworld.HelloMath", "ldiv"))
      println(vm.run("helloworld.HelloMath", "lmod"))
    }
    "float" in {
      val vm = new VirtualMachine(loadClass)
      vm.classes("helloworld.HelloMath")
      println(vm.run("helloworld.HelloMath", "fadd"))
      println(vm.run("helloworld.HelloMath", "fsub"))
      println(vm.run("helloworld.HelloMath", "fmul"))
      println(vm.run("helloworld.HelloMath", "fdiv"))
      println(vm.run("helloworld.HelloMath", "fmod"))
    }
    "double" in {
      val vm = new VirtualMachine(loadClass)
      vm.classes("helloworld.HelloMath")
      println(vm.run("helloworld.HelloMath", "dadd"))
      println(vm.run("helloworld.HelloMath", "dsub"))
      println(vm.run("helloworld.HelloMath", "dmul"))
      println(vm.run("helloworld.HelloMath", "ddiv"))
      println(vm.run("helloworld.HelloMath", "dmod"))
    }
  }

}
