package metascala.unit

import utest._

import org.objectweb.asm.Opcodes
import metascala._
import metascala.imm.Type.Prim
import metascala.Gen._
import metascala.imm.Type.Prim._
import metascala.util.Util

import TestUtil._
object  Misc extends utest.TestSuite {
  val arr = new Array[Int](2)

  def test[T](p: Prim[T])(cases: Iterable[T]) {
    chk { x: T =>
      p.write(x, Util.writer(arr, 0))
      assertEquals(p.read(Util.reader(arr, 0)), x)
    }(cases)
  }

  def tests = this {
    "making sure Prim[T] write & pops preserve the value T" - {
      "testZ" - test(Z)(Seq(true, false))
      "testB" - test(B)(30 ** Gen.intAll.toByte)
      "testC" - test(C)(30 ** Gen.intAll.toChar)
      "testS" - test(S)(30 ** Gen.intAll.toShort)
      "testF" - test(F)(30 ** java.lang.Float.intBitsToFloat(Gen.intAll))
      "testL" - test(J)(30 ** Gen.longAll)
      "testD" - test(D)(30 ** Gen.doubleAll)
    }
  }
}