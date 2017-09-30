package metascala
package full

import utest._

import java.math.BigInteger
import java.util.regex.{Matcher, Pattern}
import java.util.concurrent.atomic.{AtomicLong, AtomicInteger, AtomicBoolean}
import java.util.concurrent.{ConcurrentLinkedQueue, ConcurrentHashMap}
import java.util.concurrent.locks.ReentrantLock


import TestUtil._
object JavaLibTest extends utest.TestSuite {
  def tests = Tests {
    "sudoku" - {
      val tester = new Tester("metascala.full.Sudoku")
      tester.run[String]("run")
    }
    "stuff" - {
      val tester = new VM()
      "sorting" - tester.test {
        val arr: Array[Int] = new Array[Int](250)

        var current: Int = 94664704
        for (i <- arr.indices) {
          current = 23 * current % 100000000 + 1
          arr(i) = current % 100000
        }

        java.util.Arrays.sort(arr)
        arr(52)
      }
      "collections" - tester.test {
        val vec = new java.util.Vector[Integer]()
        for (i <- 0 until 10) {
          vec.add(i)
        }
        val map = new java.util.HashMap[Integer, String]()
        var total = 0
        for (i <- 0 until 10) {
          total = total + vec.get(i)
          map.put(vec.get(i), "" + total)
        }

        Integer.parseInt(map.get(10 / 2))
      }

      "bigInteger" - tester.test {
        val a: BigInteger = new BigInteger("1237923896771092385")
        val b: BigInteger = new BigInteger("498658982734992345912340")
        val c: BigInteger = new BigInteger("08968240235478367717203984123")

        val d: BigInteger = a.add(b)
        val e: BigInteger = d.subtract(c)
        val f: BigInteger = e.multiply(b)
        val g: BigInteger = f.divide(a)

        g.toString
      }
      "regex" - tester.test {
        val p: Pattern = Pattern.compile("\\d+([_-]\\d+)*(:? )")
        val m: Matcher = p.matcher("123_321_12 i am a cow 123_3-" + "12_990 but my ip is 192-168-1-1 lolz")

        var s: String = ""
        while (m.find) {
          s += m.group(0)
        }

        s
      }
      "atomicBooleans" - tester.test {
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
      "atomicIntegers" - tester.test {
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
      "atomicLongs" - tester.test {
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
      "randoms" - tester.test {
        val r = new java.util.Random(241231241251241123L)
        for (i <- 0 until 100) {
          r.nextLong()
        }
        r.nextLong()
      }
      "concurrentLinkedQueue" - tester.test {
        val struct = new ConcurrentLinkedQueue[Int]()
        struct.add(123)
        struct.add(456)
        val i = struct.poll()
        struct.peek() + i + struct.peek()
      }
      "locks" - tester.test {
        val lock = new ReentrantLock()
        lock.lock()
        lock.unlock()
      }
      "concurrentHashMap" - tester.test {
        val map = new ConcurrentHashMap[Int, Int]()
        map.put(123, 456)
      }
      "locales" - tester.test {
        new java.util.Locale("").toString
      }
    }
  }
}

