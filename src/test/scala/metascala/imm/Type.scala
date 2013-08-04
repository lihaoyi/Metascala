package metascala.imm

import org.scalatest.FreeSpec
import metascala.Util
import metascala.imm.Type.Prim._
import metascala.imm.Type.Prim

class TypeTest extends FreeSpec {
  "helloObj" in {
    def test(p: Prim[_]) = {
      assert(p.javaName == p.primClass.getName, s"${p.javaName} != ${p.realCls.getName}")
    }
    Seq(Z, B, C, S, I, F, J, D).map(test)
  }
}
