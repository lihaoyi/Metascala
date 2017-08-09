package metascala
package full

import org.scalatest.FreeSpec

import metascala.Gen._
import java.math.BigInteger
import java.util.regex.{Matcher, Pattern}
import java.util.concurrent.atomic.{AtomicLong, AtomicInteger, AtomicBoolean}
import java.util.concurrent.{ConcurrentLinkedQueue, ConcurrentHashMap}
import java.util.concurrent.locks.ReentrantLock


class JavaLibTest extends FreeSpec {
  implicit val intAll10 = 10 ** Gen.intAll
  import Util._
  "sudoku" in {
    val tester = new Tester("metascala.full.Sudoku")
    tester.run("run")
  }
  "stuff" - {
    val tester = new VM()
    "sorting" in tester.test{
      val arr: Array[Int] = new Array[Int](250)

      var current: Int = 94664704
      for(i <- 0 until arr.length){
        current = 23 * current % 100000000 + 1
        arr(i) = current % 100000
      }

      java.util.Arrays.sort(arr)
      arr(52)
    }
    "collections" in tester.test{
      val vec = new java.util.Vector[Integer]()
      for(i <- 0 until 10 ){
        vec.add(i)
      }
      val map = new java.util.HashMap[Integer, String]()
      var total = 0
      for(i <- 0 until 10){
        total = total + vec.get(i)
        map.put(vec.get(i), ""+total)
      }

      Integer.parseInt(map.get(10/2))
    }

    "bigInteger" in tester.test{
      val a: BigInteger = new BigInteger("1237923896771092385")
      val b: BigInteger = new BigInteger("498658982734992345912340")
      val c: BigInteger = new BigInteger("08968240235478367717203984123")

      val d: BigInteger = a.add(b)
      val e: BigInteger = d.subtract(c)
      val f: BigInteger = e.multiply(b)
      val g: BigInteger = f.divide(a)

      g.toString
    }
    "regex" in tester.test{
      val p: Pattern = Pattern.compile("\\d+([_-]\\d+)*(:? )")
      val m: Matcher = p.matcher("123_321_12 i am a cow 123_3-" + "12_990 but my ip is 192-168-1-1 lolz")

      var s: String = ""
      while (m.find) {
        s += m.group(0)
      }

      s
    }
    "atomicBooleans" in tester.test{
      val b: AtomicBoolean = new AtomicBoolean
      val values: Array[Boolean] = new Array[Boolean](4)

      b.set(true)
      values(0) = b.get
      b.compareAndSet(false, false)
      values(1) = b.get
      values(2) = b.getAndSet(false)
      b.compareAndSet(false, true)
      values(3) = b.get

      values
    }
    "atomicIntegers" in tester.test{
      val b: AtomicInteger = new AtomicInteger
      val values: Array[Int] = new Array[Int](4)

      b.set(192)
      values(0) = b.get
      b.compareAndSet(12, 3123)
      values(1) = b.get
      values(2) = b.getAndSet(31241)
      b.compareAndSet(31241, 12451)
      values(3) = b.get

      values
    }
    "atomicLongs" in tester.test{
      val b: AtomicLong = new AtomicLong
      val values: Array[Long] = new Array[Long](4)

      b.set(1921231231234124124L)
      values(0) = b.get
      b.compareAndSet(12124124164865234L, 34934198359342123L)
      values(1) = b.get
      values(2) = b.getAndSet(98172271923198732L)
      b.compareAndSet(981724127399231987L, 123517894187923123L)
      values(3) = b.get

      values
    }
    "randoms" in tester.test{
      val r = new java.util.Random(241231241251241123L)
      for(i <- 0 until 100){
        r.nextLong()
      }
      r.nextLong()
    }
    "concurrentLinkedQueue" in tester.test{
      val struct = new ConcurrentLinkedQueue[Int]()
      struct.add(123)
      struct.add(456)
      val i = struct.poll()
      struct.peek() + i + struct.peek()
    }
    "locks" in tester.test{
      val lock = new ReentrantLock()
      lock.lock()
      lock.unlock()
    }
    "concurrentHashMap" in tester.test{
      val map = new ConcurrentHashMap[Int, Int]()
      map.put(123, 456)
    }
//    "rhino" in tester.test{
//      classOf[VMBridge_jdk15].newInstance()
//      /*val cx = Context.enter()
//      val scope = cx.initStandardObjects()
//      val script = "var s = 'omg'"
//      val obj = cx.evaluateString(scope, script, "Test Script", 1, null)
//      println(obj)
//      1*/
//    }
  }

}

