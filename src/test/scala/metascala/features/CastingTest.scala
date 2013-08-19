package metascala
package features

import org.scalatest.FreeSpec

import metascala.{BufferLog, Gen, Util}
import Gen.chk
import java.awt.geom.Point2D

class CastingTest extends FreeSpec {
  import Util._

  val buffer = new BufferLog(4000)
  "if else" - {
    val tester = new VM()
    "basicCasts" in chk(tester.testFunc((x: AnyRef) =>
      try{
        val s = x.asInstanceOf[String]
        s.length()
      }catch{ case x: ClassCastException =>
        -1
      }
    ) _)(Seq(
      "omg",
      Array(1, 2, 3),
      new Object()
    ))
    "arrayCasts" in chk(tester.testFunc((x: AnyRef) =>
      try {
        val s: Array[Point2D] = x.asInstanceOf[Array[Point2D]]
        s.length
      } catch { case e: ClassCastException =>
        -1
      }
    ) _)(Seq(
      Array(1, 2, 3),
      Array("omg", "wtf", "bbq"),
      Array(new Object(), new Object()),
      Array(new Point2D.Double(), new Point2D.Double())
    ))

    "primArrayCasts" in chk(tester.testFunc((x: AnyRef) =>
      try {
        val s: Array[Int] = x.asInstanceOf[Array[Int]]
        s.length
      } catch { case e: ClassCastException =>
          -1
      }
    ) _)(Seq(
      Array(1, 2, 3),
      Array(1.0, 2.0, 3.0),
      Array("omg", "wtf", "bbq")
    ))
    "instanceOf" in chk(tester.testFunc((x: AnyRef) =>
      if (x.isInstanceOf[Point2D]) 0 else 1
    ) _)(Seq(
      "omg",
      Array(1, 2, 3),
      new Object(),
      new Point2D.Double(1, 1)
    ))
    "instanceOfArray" in chk(tester.testFunc((x: AnyRef) =>
      if (x.isInstanceOf[Array[Point2D]]) 0 else 1
    ) _)(Seq(
      Array(1, 2, 3),
      Array("omg", "wtf", "bbq"),
      Array(new Object(), new Object()),
      Array(new Point2D.Double(), new Point2D.Double())
    ))
    "instanceOfPrimArray" in chk(tester.testFunc((x: AnyRef) =>
      if (x.isInstanceOf[Array[Int]]) 0 else 1
    ) _)(Seq(
      Array(1, 2, 3),
      Array(1.0, 2.0, 3.0),
      Array("omg", "wtf", "bbq")
    ))

  }

}

