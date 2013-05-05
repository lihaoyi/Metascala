package metascala

import org.scalatest.FreeSpec
import collection.mutable
import Gen._
import metascala.opcodes.LoadStore.{Store, Ldc, Load}
import metascala.opcodes.StackManip.{BinaryBranch, UnaryBranch, BinOp, UnaryOp}
import metascala.opcodes.Misc.{Goto, ReturnVal}
import metascala.opcodes.OpCode
import metascala.imm.Method



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

  /*"hello" in {
    val cls = imm.Cls.parse(natives.Bindings.default.fileLoader("metascala/features/controlflow/Loops.class").get)
    val (blocks, localsSize) = ssa.Conversion.convertToSsa(cls.methods.last)
    println(
      blocks.toSeq
            .sortBy(_._1)
            .map(_._2.map(_.toString).reduce(_+"\n"+_))
            .reduce(_+"\n\n"+_)
    )
  }*/






}
