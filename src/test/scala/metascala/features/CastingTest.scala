package metascala.features

import org.scalatest.FreeSpec

import metascala.{BufferLog, Gen, Util}
import Gen.chk
import java.awt.geom.Point2D

class CastingTest extends FreeSpec with Util{

  val buffer = new BufferLog(4000)
  "if else" - {
    val tester = new Tester("metascala.features.Casting", buffer)

    "basicCast" in chk(tester.run("basicCast", _: AnyRef))(Seq(
      "omg",
      Array(1, 2, 3),
      new Object()
    ))
    "arrayCasts" in chk(tester.run("arrayCasts", _: AnyRef))(Seq(
      Array(1, 2, 3),
      Array("omg", "wtf", "bbq"),
      Array(new Object(), new Object()),
      Array(new Point2D.Double(), new Point2D.Double())
    ))
    "primArrayCasts" in chk(tester.run("primArrayCasts", _: AnyRef))(Seq(
      Array(1, 2, 3),
      Array(1.0, 2.0, 3.0),
      Array("omg", "wtf", "bbq")
    ))
    "instanceOf" in chk(tester.run("instanceOf", _: AnyRef))(Seq(
      "omg",
      Array(1, 2, 3),
      new Object()
    ))
    "instanceOfArray" in chk(tester.run("instanceOfArray", _: AnyRef))(Seq(
      Array(1, 2, 3),
      Array("omg", "wtf", "bbq"),
      Array(new Object(), new Object()),
      Array(new Point2D.Double(), new Point2D.Double())
    ))
    "instanceOfPrimArray" in chk(tester.run("instanceOfPrimArray", _: AnyRef))(Seq(
      Array(1, 2, 3),
      Array(1.0, 2.0, 3.0),
      Array("omg", "wtf", "bbq")
    ))

  }

}

