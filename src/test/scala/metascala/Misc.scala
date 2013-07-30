package metascala

import org.scalatest.FreeSpec
import collection.mutable
import Gen._

import metascala.imm.Method

import imm.Type.Prim
import imm.Type.Prim._
import org.objectweb.asm.{Opcodes, ClassReader}
import Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.tree.analysis._
import metascala.opcodes.{Insn, BasicBlock, Code}
import scala.collection.JavaConverters._
import metascala.opcodes.Code
import metascala.opcodes.Insn._
import metascala.StackOps.{F1, F2}
import metascala.opcodes.Insn.GetArray
import metascala.opcodes.Insn.Push
import metascala.opcodes.Insn.BinOp
import metascala.StackOps.F1
import metascala.opcodes.Insn.PutArray
import metascala.StackOps.F2
import metascala.opcodes.Code

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

  "hello" in {

  }

}
