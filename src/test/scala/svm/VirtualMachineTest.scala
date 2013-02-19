package svm

import parsing.Util
import org.scalatest.FreeSpec
import java.io.DataInputStream

class VirtualMachineTest extends FreeSpec{
  def loadClass(name: String) = {
    println("Looking for Class: " + name)
    val stream = new DataInputStream(
      getClass.getResourceAsStream(s"/${name.replace('.', '/')}.class")
    )
    val bytes = new Array[Byte](stream.available())
    stream.readFully(bytes)
    bytes
  }

  "hello math" in {
    val cls = "helloworld.HelloMath"
    val vm = new VirtualMachine(loadClass)
    vm.getClassFor(cls)
    assert(vm.run(cls, "imain") === Some(1337))
    assert(vm.run(cls, "fmain") === Some(1.337f))
    assert(vm.run(cls, "amain") === Some("i am 3l33t"))
  }

  "hello longs" in {
    val cls = "helloworld.HelloLongs"
    val vm = new VirtualMachine(loadClass)
    vm.getClassFor(cls)
    assert(vm.run(cls, "lmain") === Some(31337))
    assert(vm.run(cls, "dmain") === Some(31.337))
  }
  "basic math" - {
    "int" in {
      val cls = "helloworld.HelloMath"
      val vm = new VirtualMachine(loadClass)
      vm.getClassFor(cls)
      assert(vm.run(cls, "ineg") === Some(-13))
      assert(vm.run(cls, "iadd") === Some(17))
      assert(vm.run(cls, "isub") === Some(9))
      assert(vm.run(cls, "imul") === Some(52))
      assert(vm.run(cls, "idiv") === Some(3))
      assert(vm.run(cls, "imod") === Some(1))
    }
    "long" in {
      val cls = "helloworld.HelloLongs"
      val vm = new VirtualMachine(loadClass)
      vm.getClassFor(cls)
      assert(vm.run(cls, "lneg") === Some(-13))
      assert(vm.run(cls, "ladd") === Some(17))
      assert(vm.run(cls, "lsub") === Some(9))
      assert(vm.run(cls, "lmul") === Some(52))
      assert(vm.run(cls, "ldiv") === Some(3))
      assert(vm.run(cls, "lmod") === Some(1))
    }
    "float" in {
      val cls = "helloworld.HelloMath"
      val vm = new VirtualMachine(loadClass)
      vm.getClassFor(cls)
      assert(vm.run(cls, "fneg") === Some(-13.0f))
      assert(vm.run(cls, "fadd") === Some(17.0f))
      assert(vm.run(cls, "fsub") === Some(9.0f))
      assert(vm.run(cls, "fmul") === Some(52.0f))
      assert(vm.run(cls, "fdiv") === Some(3.25f))
      assert(vm.run(cls, "fmod") == Some(1.0f))
    }
    "double" in {
      val vm = new VirtualMachine(loadClass)
      val cls = "helloworld.HelloLongs"
      vm.getClassFor(cls)
      assert(vm.run(cls, "dneg") === Some(-13.0f))
      assert(vm.run(cls, "dadd") === Some(17.0f))
      assert(vm.run(cls, "dsub") === Some(9.0f))
      assert(vm.run(cls, "dmul") === Some(52.0f))
      assert(vm.run(cls, "ddiv") === Some(3.25f))
      assert(vm.run(cls, "dmod") == Some(1.0f))
    }

    "more int stuff" in {
      val cls = "helloworld.HelloMath"
      val vm = new VirtualMachine(loadClass)
      vm.getClassFor(cls)
      assert(vm.run(cls, "ishl") === Some(0x9abcde00))
      assert(vm.run(cls, "ishr") === Some(0x789abc))
      assert(vm.run(cls, "iushr") === Some(0x789abc))
      assert(vm.run(cls, "iand") === Some(0x7090b0d0))
      assert(vm.run(cls, "ior") === Some(0xf8fafcfe))
      assert(vm.run(cls, "ixor") === Some(0x886a4c2e))
    }

    "more long stuff" in {
      val cls = "helloworld.HelloLongs"
      val vm = new VirtualMachine(loadClass)
      vm.getClassFor(cls)
      assert(vm.run(cls, "lshl") === Some(0x56789abcdef00000L))
      assert(vm.run(cls, "lshr") === Some(0x123456789abcL))
      assert(vm.run(cls, "lushr") === Some(0x123456789abcL))
      assert(vm.run(cls, "land") === Some(0x1030507090b0d0f0L))
      assert(vm.run(cls, "lor") === Some(0xf2f4f6f8fafcfef0L))
      assert(vm.run(cls, "lxor") === Some(0xe2c4a6886a4c2e00L))
    }
  }

}
