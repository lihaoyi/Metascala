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


}
