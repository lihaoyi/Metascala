package svm.model

import org.scalatest.FreeSpec
import svm.model.OpCode

class OpCodesTest extends FreeSpec{
  "opcodes must line up" in {
    for((oc, i) <- OpCode.all.zipWithIndex) oc match{
      case o: OpCode => assert(o.id === i.toByte)
      case _ => () // ignore opcodes that need args
    }
  }
  "need to have enough opcodes: 0 to 201 makes 202 in total" in {
    assert(OpCode.all.length === 202)
  }
}

