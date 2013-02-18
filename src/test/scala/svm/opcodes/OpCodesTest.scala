package svm.opcodes

import org.scalatest.FreeSpec
import svm.parsing.opcodes.OpCodes
import svm.parsing.opcodes.OpCodeGen.OpCode
class OpCodesTest extends FreeSpec{
  "opcodes must line up" in {
    for((oc, i) <- OpCodes.all.zipWithIndex){
      assert(oc.id === i.toByte)
    }
  }
}
