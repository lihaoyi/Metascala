package metascala
package full

//import metascala.{UncaughtVmException, Gen, Util}
import org.scalatest.FreeSpec
import metascala.Util._

import collection.GenSeq
import metascala.features.Bull

object MetacircularTest{



}

class MetacircularTest extends FreeSpec {
  import Util._

  val buffer = new BufferLog(1900)
  var count = 0

  "sqrtFinder" in {
    val tester = new VM(memorySize = 280 * 1024)
    tester.test{
      val x = new VM(memorySize = 1024)
      x.invoke("metascala/features/controlflow/Loops", "sqrtFinder", Seq(5.0))
    }
  }

  "fibonacci" in {
    val tester = new VM(memorySize = 3 * 1024 * 1024)
    tester.test{
      val x = new metascala.VM()
      x.invoke("metascala/features/methods/Statics", "fibonacci", Seq(12))
    }
  }

  "inheritance" in {
    val tester = new VM(memorySize = 8 * 1014 * 1024)
    tester.test{
      val x = new metascala.VM()
      x.exec{
        val b = new Bull
        b.mooTwice
      }
    }
  }

  "bubbleSort" in {
    val tester = new VM(memorySize = 6 * 1014 * 1024)
    tester.test{
      val x = new metascala.VM()
      x.invoke("metascala/features/ArrayTest", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
        .cast[Array[Int]]
        .toSeq
    }
  }

  "getAndSet" in {
    val tester = new VM(memorySize = 15 * 1014 * 1024)
    tester.test{
      val x = new metascala.VM()
      x.invoke("metascala/features/arrays/MultiDimArrays", "getAndSet")
    }
  }

  "multiCatch" in {

    val tester = new VM(memorySize = 16 * 1014 * 1024)
    tester.test{
      val x = new metascala.VM()
      x.invoke("metascala/features/exceptions/Exceptions", "multiCatch", Seq(2))
    }
  }
  "doubleMetaOne" in {
    val tester = new VM(memorySize = 16 * 1014 * 1024)
    tester.test{
      val x = new VM()
      x.exec{
        val y = new VM()
        y.invoke("metascala/features/controlflow/Loops", "sqrtFinder", Seq(5.0))
      }
    }

  }
}

