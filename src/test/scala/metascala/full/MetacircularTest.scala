package metascala
package full

import org.scalatest.FreeSpec
import metascala.features.Bull


class MetacircularTest extends FreeSpec {
  import Util._

  val buffer = new BufferLog(1900)
  var count = 0

  "sqrtFinder" in {
    new VM(memorySize = 225 * 1024).test{
      val x = new VM(memorySize = 1024)
      x.invoke("metascala/features/controlflow/Loops", "sqrtFinder", Seq(5.0))
    }
  }

  "fibonacci" in {
    new VM(memorySize = 3 * 1024 * 1024).test{
      val x = new metascala.VM()
      x.invoke("metascala/features/methods/Statics", "fibonacci", Seq(12))
    }
  }

  "inheritance" in {
    val vm = new VM(memorySize = 8 * 1014 * 1024)
    vm.test{
      val vm = new VM()
      val x = vm.exec{
        val b = new Bull
        b.mooTwice
      }
      println(vm.threads(0).count)
      x
    }
    println(vm.threads(0).count)
  }

  "bubbleSort" in {
    new VM(memorySize = 24 * 1014 * 1024).test{
      val x = new metascala.VM()
      x.invoke("metascala/features/ArrayTest", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
        .cast[Array[Int]]
        .toSeq
    }
  }

  "getAndSet" in {
    new VM(memorySize = 15 * 1014 * 1024).test{
      val x = new metascala.VM()
      x.invoke("metascala/features/arrays/MultiDimArrays", "getAndSet")
    }
  }

  "multiCatch" in {
    new VM(memorySize = 16 * 1014 * 1024).test{
      val x = new metascala.VM()
      x.invoke("metascala/features/exceptions/Exceptions", "multiCatch", Seq(2))
    }
  }

  "reflectField" in {
    val vm = new VM(memorySize = 16 * 1014 * 1024)
    vm.test{
      val vm = new VM()
      vm.exec{
        val string = new String("i am a cow")
        val f = classOf[String].getDeclaredField("value")
        f.setAccessible(true)
        f.set(string, Array('o', 'm', 'g'))
        f.get(string)
      }
    }
  }

//  "predef" in {
//    new VM(memorySize = 16 * 1014 * 1024).test{
//      new VM(log = x => println(x)).exec{
//        Predef
//      }
//    }
//  }

}


