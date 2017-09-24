package metascala
package features

import utest._
import metascala.TestUtil._
import java.awt.Point

import scala.util.Random

object MethodTest extends utest.TestSuite {

  def tests = Tests {
    "natives" - {
      val tester = new VM()
      "intBitsToFloat" - {
        for (n <- Iterator.fill(10)(Random.nextInt())) {
          tester.test(java.lang.Float.intBitsToFloat(n))
        }
      }
      "currentTimeMillis" - tester.test(
        System.currentTimeMillis() / 100000
      )
      "inheritedNative" - tester.test(
        "omg".getClass().getName()
      )
      "arrayCopy" - tester.test {
        val x = new Array[Int](5)
        x(0) = 1
        x(1) = 2
        x(2) = 3
        x(3) = 4
        x(4) = 5


        val y = new Array[Int](5)
        y(0) = 1
        y(1) = 2
        y(2) = 3
        y(3) = 4
        y(4) = 5

        System.arraycopy(x, 1, y, 2, 2)

        y(0) * 10000 + y(1) * 1000 + y(2) * 100 * y(3) * 10 + y(4)
      }
    }
    "objects" - {
      val tester = new VM()
      "helloWorld" - tester.test {
        val d = new DumbObject(5)
        d.getTwoN
      }
      "stringEquals" - {
        val a = 0
        val b = "0"
        tester.test("" + a == b)
      }
      "inheritance" - tester.test {
        val d = new DumbObjectSubClass(5)
        d.getTwoN
      }

      "points" - (
        for (i <- Iterator.fill(10)(Random.nextInt())) {
          tester.test {
            val p = new Point(i, i)
            p.getX
          }
        }
        )
      "points2" - (
        for (n <- Iterator.fill(10)(Random.nextInt())) {
          tester.test {
            val p = new Point(10, 10)
            p.translate(5, -5)
            p.setLocation(p.x * n, p.y * n)
            p.setLocation(p.getY, p.getX)
            p.translate(n, -n)
            p.distanceSq(0, n)
          }
        }
        )
    }
  }
}
class DumbObjectSubClass(n: Int) extends DumbObject(n * 2)

class DumbObject(val n: Int) {

  def getTwoN: Int = {
    return n * 2
  }
}

