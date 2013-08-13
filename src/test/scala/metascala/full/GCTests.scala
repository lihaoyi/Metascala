package metascala.full

import metascala.Util
import org.scalatest.FreeSpec
object GCTestsBasic{
  def helloObj(n: Int) = {
    var i = 0
    while(i < n){
      val p = new Object()
      i += 1
    }
    i
  }
  def helloArr(n: Int) = {
    var i = 0
    val p = new Array[Int](2)
    p(0) = 5
    p(1) = 6
    val p2 = new Array[Array[Int]](1)
    p2(0) = p

    while(i < n){
      val p = new Array[Int](3)
      p(0) = 9
      p(1) = 8
      p(2) = 7
      i += 1
    }
    p2(0)(1)
  }

  def chain(n: Int) = {
    var i = 0
    var front = new Cons(1, null)
    var back = new Cons(5, new Cons(4, new Cons(3, new Cons(2, front))))
    while (i < n){
      front.next = new Cons(front.value + 1, null)
      front = front.next
      back = back.next
      i += 1
    }
    front.value
  }
  def interned(n: Int) = {
    var i = 0
    while(i < n){
      val p = new Object()
      i += 1
    }
    "aaaaa"
  }
  def mixed() = {
    var i = 0
    while(i < 100){
      i += 1
      helloObj(i)
      helloArr(i)
      chain(i)
      interned(i)
    }
    interned(10)
  }
}
class Cons(val value: Int, var next: Cons)

class GCTests extends FreeSpec with Util{

  "helloObj" in {
    for{
      memory <- List(20, 30, 67, 121)
      count <- List(0, 1, 5, 19, 30, 67)
    }{
      val tester = new Tester("metascala.full.GCTestsBasic", memorySize = memory)
      tester.run("helloObj", count)
    }
  }

  "helloArr" in {
    for{
      memory <- List(30, 65, 93, 123)
      count <- List(0, 3, 9, 12, 30)
    }{
      println(memory + " " + count)
      val tester = new Tester("metascala.full.GCTestsBasic", memorySize = memory)
      tester.run("helloArr", count)
    }
  }

  "chain" in {
    for {
      memory <- 40 to 45 by 2
      count <- 20 to 30 by 3
    }{
      val tester = new Tester("metascala.full.GCTestsBasic", memorySize = 40)
      tester.run("chain", count)
    }
  }

  "interned" in {
    for {
      memory <- 40 to 45 by 2
      count <- 20 to 30 by 3
    }{
      val tester = new Tester("metascala.full.GCTestsBasic", memorySize = memory)
      tester.run("interned", count)
    }
  }
  "mixed" in {
    for {
      memory <- Seq(500)
    }{
      val tester = new Tester("metascala.full.GCTestsBasic", memorySize = memory)
      tester.run("mixed")
    }
  }
  "parseClass" in {
    val tester = new Tester("metascala.full.ScalaLib", memorySize = 9 * 1024)
    for(i <- 1 to 5) tester.run("parseClass")
  }
  "gcInterruptNegative" in {
    val tester = new Tester("metascala.full.ScalaLib", memorySize = 10)
    implicit val vm = tester.svm
    import metascala.pimpedString

    vm.alloc("java/lang/Object".allocObj()(_))
    vm.alloc("java/lang/Object".allocObj()(_))
    vm.alloc("java/lang/Object".allocObj()(_))
    println(vm.heap.dump(0, 20))
    vm.heap.collect(vm.heap.start)

    // everything got GCed, nothing in the second memory space
    assert(vm.heap.memory.slice(10, 20).forall(_ == 0))
  }
  "gcInterruptPositive" in {
    val tester = new Tester("metascala.full.ScalaLib", memorySize = 10)
    implicit val vm = tester.svm
    import metascala.pimpedString
    vm.alloc{ implicit r =>
      val p1 = "java/lang/Object".allocObj()
      val p2 = "java/lang/Object".allocObj()
      val p3 = "java/lang/Object".allocObj()
      println(vm.heap.dump(0, 20))
      vm.heap.collect(vm.heap.start)
      println(vm.heap.dump(0, 20))
      // everything got GCed
    }
  }
}