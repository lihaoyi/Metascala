package sm.imm

import org.scalatest.FreeSpec

class OpCodesTest extends FreeSpec{

  "need to have enough opcodes: 0 to 201 makes 202 in total" in {
    assert(OpCode.all.length === 202)
  }
}

