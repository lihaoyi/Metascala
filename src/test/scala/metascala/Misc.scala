package metascala

import org.scalatest.FreeSpec
import collection.mutable
import Gen._
class Misc extends FreeSpec with Util{
  val arr = new Array[Int](2)
  def test[T](p: Prim[T])(cases: Iterable[T]){
    chk{ x: T =>
      p.write(x, writer(arr, 0))
      assert(p.read(reader(arr, 0)) === x)
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
