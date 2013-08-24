package metascala
package features

import org.scalatest.FreeSpec

import metascala.{Gen, Util}
import metascala.Gen._
import java.awt.Point

class MethodTest extends FreeSpec {
  import Util._

  implicit val intAll10 = 10 ** Gen.intAll

  "static" - {
    val tester = new Tester("metascala.features.methods.Statics")
    "helloWorld" in chk(tester.run("helloWorld", _: Int))

    "helloWorld2" in chk(tester.run("helloWorld2", _: Int, _: Int))
    "tailFactorial" in chk(tester.run("tailFactorial", _: Int))(Seq(2, 5, 10, 20, 50))
    "fibonacci" in chk(tester.run("fibonacci", _: Int))(Seq(2, 5, 10))
    "callAtPhiBoundary" in tester.run("callAtPhiBoundary", 0)
  }
  "natives" - {
    val tester = new VM()
    "intBitsToFloat" in {
      for(n <- intAll10){
        tester.test(java.lang.Float.intBitsToFloat(n))
      }
    }
    "currentTimeMillis" in tester.test(
      System.currentTimeMillis() / 100000
    )
    "inheritedNative" in tester.test(
      "omg".getClass().getName()
    )
    "arrayCopy" in tester.test{
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
    "helloWorld" in tester.test{
      val d = new DumbObject(5)
      d.getTwoN
    }
    "stringEquals" in {
      val a = 0
      val b = "0"
      tester.test(""+a == b)
    }
    "inheritance" in tester.test{
      val d = new DumbObjectSubClass(5)
      d.getTwoN
    }

    "points" in (
      for(i <- intAll10){
        tester.test{
          val p = new Point(i, i)
          p.getX
        }
      }
    )
    "points2" in (
      for(n <- intAll10){
        tester.test{
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

class DumbObjectSubClass(n: Int) extends DumbObject(n * 2)

class DumbObject(val n: Int) {

  def getTwoN: Int = {
    return n * 2
  }
}

