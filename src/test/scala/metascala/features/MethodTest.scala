package metascala
package features

import org.scalatest.FreeSpec

import metascala.{Gen, Util}
import metascala.Gen._
import java.awt.Point

class MethodTest extends FreeSpec with Util{


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
    "intBitsToFloat" in chk(tester.testFunc((n: Int) =>
      java.lang.Float.intBitsToFloat(n)) _
    )
    "currentTimeMillis" in tester.testFunc(() =>
      System.currentTimeMillis() / 100000
    )
    "inheritedNative" in tester.testFunc(() =>
      "omg".getClass().getName()
    )
    "arrayCopy" in tester.testFunc{() =>
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
    "helloWorld" in tester.testFunc{(n: Int) =>
      val d = new DumbObject(n)
      d.getTwoN
    }(5)
    "stringEquals" in chk(
      tester.testFunc((a: Int, b: String) => ""+a == b) _
    )(
      Seq(0), Seq("0")
    )
    "inheritance" in tester.testFunc{(n: Int) =>
      val d = new DumbObjectSubClass(n)
      d.getTwoN
    }(5)

    "points" in chk(
      tester.testFunc{(n: Int) =>
        val p = new Point(10, 10)
        p.getX
      } _
    )
    "points2" in chk(
      tester.testFunc{(n: Int) =>
        val p = new Point(10, 10)
        p.translate(5, -5)
        p.setLocation(p.x * n, p.y * n)
        p.setLocation(p.getY, p.getX)
        p.translate(n, -n)
        p.distanceSq(0, n)
      } _
    )
  }

}

class DumbObjectSubClass(n: Int) extends DumbObject(n * 2)

class DumbObject(val n: Int) {

  def getTwoN: Int = {
    return n * 2
  }
}

