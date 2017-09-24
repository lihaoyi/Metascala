package metascala.unit

import utest._
import metascala._
import metascala.imm.Type.Prim
import metascala.imm.Type.Prim._
import metascala.util.Util
import TestUtil._

import scala.util.Random
object  Misc extends utest.TestSuite {
  val arr = new Array[Int](2)

  def test[T](p: Prim[T])(cases: Iterator[T]) {
    cases.foreach { x =>
      p.write(x, Util.writer(arr, 0))
      assertEquals(p.read(Util.reader(arr, 0)), x)
    }
  }

  def tests = Tests {
    "making sure Prim[T] write & pops preserve the value T" - {
      "testZ" - test(Z)(Iterator(true, false))
      "testB" - test(B)(Iterator.fill(30)(Random.nextInt.toByte))
      "testC" - test(C)(Iterator.fill(30)(Random.nextInt.toChar))
      "testS" - test(S)(Iterator.fill(30)(Random.nextInt.toShort))
      "testF" - test(F)(Iterator.fill(30)(Random.nextFloat()))
      "testL" - test(J)(Iterator.fill(30)(Random.nextLong))
      "testD" - test(D)(Iterator.fill(30)(Random.nextDouble()))
    }
  }
}