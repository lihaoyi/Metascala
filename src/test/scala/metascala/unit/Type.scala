package metascala.unit

import utest._
import metascala.TestUtil
import metascala.imm.Type.Prim._
import metascala.imm.Type.Prim

object Type extends utest.TestSuite {
  def tests = this {
    "helloObj" - {
      def test(p: Prim[_]) = {
        assert(p.javaName == p.primClass.getName)
      }

      Seq(Z, B, C, S, I, F, J, D).map(test)
    }
  }
}