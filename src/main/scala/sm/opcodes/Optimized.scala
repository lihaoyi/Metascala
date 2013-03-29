package sm
package opcodes

import sm.imm.{Type}
import sm.vrt

object Optimized {
  case class New(clsId: Int) extends OpCode{
    def op(vt: VmThread) = {
      import vt.vm._
      vt.push(new vrt.Obj(vt.vm.Classes.clsIndex(clsId))(vt.vm))
    }
  }

  case class InvokeStatic(thunk: Seq[vrt.StackVal] => Unit, argCount: Int) extends OpCode{
    def op(vt: VmThread) = {
      val args = for(i <- 0 until argCount) yield vt.frame.stack.pop()
      thunk(args.toSeq.reverse)
    }
  }

  case class GetStatic(field: rt.Var) extends OpCode{
    def op(vt: VmThread) = vt.push(field().toStackVal)
  }
  case class PutStatic(field: rt.Var) extends OpCode{
    def op(vt: VmThread) = field() = vt.pop
  }
}
