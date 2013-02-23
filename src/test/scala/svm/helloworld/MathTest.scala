package svm.helloworld


import org.scalatest.FreeSpec
import java.io.DataInputStream
import svm.Util._
import svm.Util
import scala.Some

class MathTest extends FreeSpec{
  import Util.loadClass

  "hello math" - {
    val vm = new SingleClassVirtualMachine("helloworld.math.HelloMath", loadClass)
    "imain" in assert(vm.run("imain") === Some(1337))
    "fmain" in assert(vm.run("fmain") === Some(1.337f))
    "amain" in assert(vm.run("amain") === Some("i am 3l33t"))
  }

  "hello longs" - {
    val vm = new SingleClassVirtualMachine("helloworld.math.HelloLongs", loadClass)
    "lmain" in assert(vm.run("lmain") === Some(31337))
    "dmain" in assert(vm.run("dmain") === Some(31.337))
  }

  "basic math" - {
    "int" - {
      val vm = new SingleClassVirtualMachine("helloworld.math.HelloMath", loadClass)
      "ineg" in assert(vm.run("ineg") === Some(-13))
      "iadd" in assert(vm.run("iadd") === Some(17))
      "isub" in assert(vm.run("isub") === Some(9))
      "imul" in assert(vm.run("imul") === Some(52))
      "idiv" in assert(vm.run("idiv") === Some(3))
      "imod" in assert(vm.run("imod") === Some(1))
    }
    "long" - {
      val vm = new SingleClassVirtualMachine("helloworld.math.HelloLongs", loadClass)
      "lneg" in assert(vm.run("lneg") === Some(-13))
      "ladd" in assert(vm.run("ladd") === Some(17))
      "lsub" in assert(vm.run("lsub") === Some(9))
      "lmul" in assert(vm.run("lmul") === Some(52))
      "ldiv" in assert(vm.run("ldiv") === Some(3))
      "lmod" in assert(vm.run("lmod") === Some(1))
    }
    "float" - {
      
      val vm = new SingleClassVirtualMachine("helloworld.math.HelloMath", loadClass)
      "fneg" in assert(vm.run("fneg") === Some(-13.0f))
      "fadd" in assert(vm.run("fadd") === Some(17.0f))
      "fsub" in assert(vm.run("fsub") === Some(9.0f))
      "fmul" in assert(vm.run("fmul") === Some(52.0f))
      "fdiv" in assert(vm.run("fdiv") === Some(3.25f))
      "fmod" in assert(vm.run("fmod") == Some(1.0f))
    }
    "double" - {
      val vm = new SingleClassVirtualMachine("helloworld.math.HelloLongs", loadClass)
      "dneg" in assert(vm.run("dneg") === Some(-13.0f))
      "dadd" in assert(vm.run("dadd") === Some(17.0f))
      "dsub" in assert(vm.run("dsub") === Some(9.0f))
      "dmul" in assert(vm.run("dmul") === Some(52.0f))
      "ddiv" in assert(vm.run("ddiv") === Some(3.25f))
      "dmod" in assert(vm.run("dmod") == Some(1.0f))
    }

    "more int stuff" - {
      val vm = new SingleClassVirtualMachine("helloworld.math.HelloMath", loadClass)
      "ishl" in assert(vm.run("ishl") === Some(0x9abcde00))
      "ishr" in assert(vm.run("ishr") === Some(0x789abc))
      "iushr" in assert(vm.run("iushr") === Some(0x789abc))
      "iand" in assert(vm.run("iand") === Some(0x7090b0d0))
      "ior" in assert(vm.run("ior") === Some(0xf8fafcfe))
      "ixor" in assert(vm.run("ixor") === Some(0x886a4c2e))
    }

    "more long stuff" - {
      val vm = new SingleClassVirtualMachine("helloworld.math.HelloLongs", loadClass)
      "lshl" in assert(vm.run("lshl") === Some(0x56789abcdef00000L))
      "lshr" in assert(vm.run("lshr") === Some(0x123456789abcL))
      "lushr" in assert(vm.run("lushr") === Some(0x123456789abcL))
      "land" in assert(vm.run("land") === Some(0x1030507090b0d0f0L))
      "lor" in assert(vm.run("lor") === Some(0xf2f4f6f8fafcfef0L))
      "lxor" in assert(vm.run("lxor") === Some(0xe2c4a6886a4c2e00L))
    }
  }

}

