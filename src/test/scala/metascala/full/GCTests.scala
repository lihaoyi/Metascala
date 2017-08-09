package metascala
package full

import metascala.{BufferLog, Util}
import metascala.Util._
import org.scalatest.FreeSpec

object StaticHolder{
  val x = new Array[Int](2)
}
class Cons(val value: Int, var next: Cons)

class GCTests extends FreeSpec {
  import Util._

  "helloObj" in {
    for{
      memory <- List(30, 67, 121)
      count <- List(0, 1, 5, 19, 30, 67)
    }{
      val tester = new VM(memorySize = memory)
      tester.test{
        var i = 0
        while(i < count){
          val p = new Object()
          i += 1
        }
        i
      }
    }
  }

  "helloArr" in {
    for{
      memory <- List(30, 65, 93, 123)
      count <- List(0, 3, 9, 12, 30)
    }{
      println(memory + " " + count)
      val tester = new VM(memorySize = memory)
      tester.test{
        var i = 0
        val p = new Array[Int](2)
        p(0) = 5
        p(1) = 6
        val p2 = new Array[Array[Int]](1)
        p2(0) = p

        while(i < count){
          val p = new Array[Int](3)
          p(0) = 9
          p(1) = 8
          p(2) = 7
          i += 1
        }
        p2(0)(1)
      }
    }
  }

  "chain" in {
    for {
      memory <- 40 to 45 by 2
      count <- 20 to 30 by 3
    }{
      val tester = new VM(memorySize = 40)
      tester.test{
        var i = 0
        var front = new Cons(1, null)
        var back = new Cons(5, new Cons(4, new Cons(3, new Cons(2, front))))
        while (i < count){
          front.next = new Cons(front.value + 1, null)
          front = front.next
          back = back.next
          i += 1
        }
        front.value
      }
    }
  }

  "interned" in {
    for {
      memory <- 40 to 45 by 2
      count <- 20 to 30 by 3
    }{
      val tester = new VM(memorySize = memory)
      tester.test{
        var i = 0
        while(i < count){
          val p = new Object()
          i += 1
        }
        "aaaaa"
      }
    }
  }

  "static" in {
    val tester = new VM(memorySize = 40)
    tester.test{
      var o = StaticHolder.x
      var i = 0
      while(i < 10){
        o = new Array[Int](2)
        i += 1
      }
      o = StaticHolder.x
      o
    }
  }
  "parseClass" in {
    val bl = new BufferLog(4000)
    val tester = new Tester("metascala.full.ScalaLib", log=bl, memorySize = 12 * 1024)
    try{
      for(i <- 1 to 5) tester.run("parseClass")
    } catch{case e =>
//      bl.lines.foreach(println)
      throw e
    }
  }

  "gcInterrupt" in {
    val tester = new Tester("metascala.full.ScalaLib", memorySize = 10)
    implicit val vm = tester.svm
    import metascala.pimpedString
    var initialSet: Set[Int] = null
    val (p1, p2, p3) = vm.alloc{ implicit r =>
      val p1 = "java/lang/Object".allocObj()
      val p2 = "java/lang/Object".allocObj()
      val p3 = "java/lang/Object".allocObj()
      initialSet = Set(p1(), p2(), p3())
      assert(initialSet.forall(p => vm.heap(p) == -1))
      vm.heap.collect(vm.heap.start)
      // These guys got moved together to the new space (maybe in a new order)
      assert(Set(p1(), p2(), p3()) == initialSet.map(_+10))
      assert(Set(p1, p2, p3).forall(p => vm.heap(p()) == -1))
      vm.heap.collect(vm.heap.start)
      assert(Set(p1(), p2(), p3()) == initialSet)
      assert(Set(p1, p2, p3).forall(p => vm.heap(p()) == -1))
      (p1, p2, p3)
    }

    vm.heap.collect(vm.heap.start)
    vm.heap.collect(vm.heap.start)
    // after exiting the `alloc{}` block, the refs become meaningless
    // as the things they are pointing to get GCed
    assert(Set(p1(), p2(), p3()) == initialSet)
    assert(Set(p1, p2, p3).forall(p => vm.heap(p()) == 0))
  }
}