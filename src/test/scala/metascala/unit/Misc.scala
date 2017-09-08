package metascala.unit

import org.scalatest.FreeSpec

import org.objectweb.asm.Opcodes
import metascala._
import metascala.imm.Type.Prim
import metascala.Gen._
import metascala.imm.Type.Prim._
import metascala.util.Util

class Misc extends FreeSpec {
  import TestUtil._
  val arr = new Array[Int](2)
  def test[T](p: Prim[T])(cases: Iterable[T]){
    chk{ x: T =>
      p.write(x, Util.writer(arr, 0))
      assertEquals(p.read(Util.reader(arr, 0)), x)
    }(cases)
  }
  "making sure Prim[T] write & pops preserve the value T" - {
    "testZ" in test(Z)(Seq(true, false))
    "testB" in test(B)(30 ** Gen.intAll.toByte)
    "testC" in test(C)(30 ** Gen.intAll.toChar)
    "testS" in test(S)(30 ** Gen.intAll.toShort)
    "testF" in test(F)(30 ** java.lang.Float.intBitsToFloat(Gen.intAll))
    "testL" in test(J)(30 ** Gen.longAll)
    "testD" in test(D)(30 ** Gen.doubleAll)
  }
}
