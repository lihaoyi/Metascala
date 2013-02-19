package svm

import org.scalatest.FreeSpec
import java.io.DataInputStream
import svm.Util.SingleClassVirtualMachine

class VirtualMachineTest extends FreeSpec{
  import Util.loadClass

  "hello math" in {
    val vm = new SingleClassVirtualMachine("helloworld.HelloMath", loadClass)

    assert(vm.run("imain") === Some(1337))
    assert(vm.run("fmain") === Some(1.337f))
    assert(vm.run("amain") === Some("i am 3l33t"))
  }

  "hello longs" in {

    val vm = new SingleClassVirtualMachine("helloworld.HelloLongs", loadClass)
    assert(vm.run("lmain") === Some(31337))
    assert(vm.run("dmain") === Some(31.337))
  }

  "basic math" - {
    "int" in {
      
      val vm = new SingleClassVirtualMachine("helloworld.HelloMath", loadClass)
      assert(vm.run("ineg") === Some(-13))
      assert(vm.run("iadd") === Some(17))
      assert(vm.run("isub") === Some(9))
      assert(vm.run("imul") === Some(52))
      assert(vm.run("idiv") === Some(3))
      assert(vm.run("imod") === Some(1))
    }
    "long" in {
      val vm = new SingleClassVirtualMachine("helloworld.HelloLongs", loadClass)
      assert(vm.run("lneg") === Some(-13))
      assert(vm.run("ladd") === Some(17))
      assert(vm.run("lsub") === Some(9))
      assert(vm.run("lmul") === Some(52))
      assert(vm.run("ldiv") === Some(3))
      assert(vm.run("lmod") === Some(1))
    }
    "float" in {
      
      val vm = new SingleClassVirtualMachine("helloworld.HelloMath", loadClass)
      assert(vm.run("fneg") === Some(-13.0f))
      assert(vm.run("fadd") === Some(17.0f))
      assert(vm.run("fsub") === Some(9.0f))
      assert(vm.run("fmul") === Some(52.0f))
      assert(vm.run("fdiv") === Some(3.25f))
      assert(vm.run("fmod") == Some(1.0f))
    }
    "double" in {
      val vm = new SingleClassVirtualMachine("helloworld.HelloLongs", loadClass)
      assert(vm.run("dneg") === Some(-13.0f))
      assert(vm.run("dadd") === Some(17.0f))
      assert(vm.run("dsub") === Some(9.0f))
      assert(vm.run("dmul") === Some(52.0f))
      assert(vm.run("ddiv") === Some(3.25f))
      assert(vm.run("dmod") == Some(1.0f))
    }

    "more int stuff" in {
      val vm = new SingleClassVirtualMachine("helloworld.HelloMath", loadClass)
      assert(vm.run("ishl") === Some(0x9abcde00))
      assert(vm.run("ishr") === Some(0x789abc))
      assert(vm.run("iushr") === Some(0x789abc))
      assert(vm.run("iand") === Some(0x7090b0d0))
      assert(vm.run("ior") === Some(0xf8fafcfe))
      assert(vm.run("ixor") === Some(0x886a4c2e))
    }

    "more long stuff" in {
      val vm = new SingleClassVirtualMachine("helloworld.HelloLongs", loadClass)
      assert(vm.run("lshl") === Some(0x56789abcdef00000L))
      assert(vm.run("lshr") === Some(0x123456789abcL))
      assert(vm.run("lushr") === Some(0x123456789abcL))
      assert(vm.run("land") === Some(0x1030507090b0d0f0L))
      assert(vm.run("lor") === Some(0xf2f4f6f8fafcfef0L))
      assert(vm.run("lxor") === Some(0xe2c4a6886a4c2e00L))
    }
  }

}
