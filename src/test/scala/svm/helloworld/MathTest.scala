package svm.helloworld


import org.scalatest.FreeSpec
import java.io.DataInputStream
import svm.Util._
import svm.Util
import scala.Some

class MathTest extends FreeSpec{
  import Util.loadClass

  "hello math" - {
    val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloMath", loadClass)
    "imain" in assert(vm.run("imain") === 1337)
    "fmain" in assert(vm.run("fmain") === 1.337f)
    "amain" in assert(vm.run("amain") === "i am 3l33t")
  }

  "hello longs" - {
    val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloLongs", loadClass)
    "lmain" in assert(vm.run("lmain") === 31337)
    "dmain" in assert(vm.run("dmain") === 31.337)
  }

  "basic math" - {
    "int" - {
      val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloMath", loadClass)
      "ineg" in assert(vm.run("ineg") === -13)
      "iadd" in assert(vm.run("iadd") === 17)
      "isub" in assert(vm.run("isub") === 9)
      "imul" in assert(vm.run("imul") === 52)
      "idiv" in assert(vm.run("idiv") === 3)
      "imod" in assert(vm.run("imod") === 1)
    }
    "long" - {
      val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloLongs", loadClass)
      "lneg" in assert(vm.run("lneg") === -13)
      "ladd" in assert(vm.run("ladd") === 17)
      "lsub" in assert(vm.run("lsub") === 9)
      "lmul" in assert(vm.run("lmul") === 52)
      "ldiv" in assert(vm.run("ldiv") === 3)
      "lmod" in assert(vm.run("lmod") === 1)
    }
    "float" - {
      
      val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloMath", loadClass)
      "fneg" in assert(vm.run("fneg") === -13.0f)
      "fadd" in assert(vm.run("fadd") === 17.0f)
      "fsub" in assert(vm.run("fsub") === 9.0f)
      "fmul" in assert(vm.run("fmul") === 52.0f)
      "fdiv" in assert(vm.run("fdiv") === 3.25f)
      "fmod" in assert(vm.run("fmod") == 1.0f)
    }
    "double" - {
      val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloLongs", loadClass)
      "dneg" in assert(vm.run("dneg") === -13.0f)
      "dadd" in assert(vm.run("dadd") === 17.0f)
      "dsub" in assert(vm.run("dsub") === 9.0f)
      "dmul" in assert(vm.run("dmul") === 52.0f)
      "ddiv" in assert(vm.run("ddiv") === 3.25f)
      "dmod" in assert(vm.run("dmod") == 1.0f)
    }

    "more int stuff" - {
      val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloMath", loadClass)
      "ishl" in assert(vm.run("ishl") === 0x9abcde00)
      "ishr" in assert(vm.run("ishr") === 0x789abc)
      "iushr" in assert(vm.run("iushr") === 0x789abc)
      "iand" in assert(vm.run("iand") === 0x7090b0d0)
      "ior" in assert(vm.run("ior") === 0xf8fafcfe)
      "ixor" in assert(vm.run("ixor") === 0x886a4c2e)
    }

    "more long stuff" - {
      val vm = new SingleClassVirtualMachine("svm.helloworld.math.HelloLongs", loadClass)
      "lshl" in assert(vm.run("lshl") === 0x56789abcdef00000L)
      "lshr" in assert(vm.run("lshr") === 0x123456789abcL)
      "lushr" in assert(vm.run("lushr") === 0x123456789abcL)
      "land" in assert(vm.run("land") === 0x1030507090b0d0f0L)
      "lor" in assert(vm.run("lor") === 0xf2f4f6f8fafcfef0L)
      "lxor" in assert(vm.run("lxor") === 0xe2c4a6886a4c2e00L)
    }
  }

}

