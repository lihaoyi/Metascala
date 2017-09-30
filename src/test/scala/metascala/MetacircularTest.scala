package metascala

import utest._
import metascala.features.Bull


import TestUtil._
object MetacircularTest extends utest.TestSuite {
  def tests = Tests {
    "sqrtFinder" - {
      new VM(memorySize = 500 * 1024).test {
        val x = new VM(memorySize = 1024)
        x.invokeSafe[Double]("metascala.features.controlflow.Loops", "sqrtFinder", Seq(5.0))
      }
    }

    "fibonacci" - {
      new VM(memorySize = 4 * 1024 * 1024).test {
        val x = new metascala.VM()
        x.invokeSafe[Int]("metascala.features.methods.Statics", "fibonacci", Seq(12))
      }
    }

    "inheritance" - {
      val vm = new VM(memorySize = 8 * 1014 * 1024)
      vm.test {
        val vm = new VM()
        val x = vm.exec {
          val b = new Bull
          b.mooTwice
        }
        x
      }
    }

    "bubbleSort" - {
      new VM(memorySize = 24 * 1014 * 1024).test {
        val x = new metascala.VM()
        x.invokeSafe[Array[Int]]("metascala.features.ArrayTest", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
          .asInstanceOf[Array[Int]]
      }
    }

    "getAndSet" - {
      new VM(memorySize = 15 * 1014 * 1024).test {
        val x = new metascala.VM()
        x.invokeSafe[Int]("metascala.features.arrays.MultiDimArrays", "getAndSet")
      }
    }

    "multiCatch" - {
      new VM(memorySize = 16 * 1014 * 1024).test {
        val x = new metascala.VM()
        x.invokeSafe[Int]("metascala.features.exceptions.Exceptions", "multiCatch", Seq(2))
      }
    }

    "reflectField" - {
      val vm = new VM(memorySize = 16 * 1014 * 1024)
      vm.test {
        val vm = new VM()
        vm.exec {
          val string = new String("i am a cow")
          val f = classOf[String].getDeclaredField("value")
          f.setAccessible(true)
          f.set(string, Array('o', 'm', 'g'))
          f.get(string).asInstanceOf[Array[Char]]
        }
      }
    }

    //  "predef" - {
    //    new VM(memorySize = 16 * 1014 * 1024).test{
    //      new VM(log = x => println(x)).exec{
    //        Predef
    //      }
    //    }
    //  }
  }

}

