package sm.opcodes

import sm.imm.{Type}
import sm.vrt

object Optimized {
  case class New(clsId: Int) extends OpCode{
    def op = implicit vt => {
      import vt.vm._
      vt.push(new vrt.Obj(vt.vm.Classes.clsIndex(clsId))(vt.vm))
    }
  }

  case class InvokeStatic(thunk: Seq[vrt.StackVal] => Unit, argCount: Int) extends OpCode{
    def op = vt => {
      val args = for(i <- 0 until argCount) yield vt.frame.stack.pop()
      thunk(args.toSeq.reverse)
    }
  }
}
