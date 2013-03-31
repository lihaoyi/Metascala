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

  case class InvokeStatic(clsIndex: Int, methodIndex: Int, argCount: Int) extends OpCode{
    def op(vt: VmThread) = {
      val args = for(i <- 0 until argCount) yield vt.frame.stack.pop()
      vt.prepInvoke(clsIndex, methodIndex, args.toSeq.reverse)
    }
  }

  case class InvokeVirtual(clsIndex: Int, methodIndex: Int, argCount: Int) extends OpCode{
    def op(vt: VmThread) = {
      val args = for(i <- 0 until (argCount+1)) yield vt.frame.stack.pop()
      ensureNonNull(vt, args.last){

        val objCls =
          args.last match{
            case a: vrt.Obj => a.cls
            case _ => vt.vm.Classes.clsIndex(clsIndex)
          }
        val (realCls, realIndex) = objCls.methodList(methodIndex)
        vt.prepInvoke(Option(realCls).map(_.index).getOrElse(-1) , realIndex, args.toSeq.reverse)
      }

    }
  }

  case class GetStatic(field: rt.Var) extends OpCode{
    def op(vt: VmThread) = vt.push(field().toStackVal)
  }
  case class PutStatic(field: rt.Var) extends OpCode{
    def op(vt: VmThread) = field() = vt.pop
  }
  case class GetField(index: Int) extends OpCode{
    def op(vt: VmThread) = {
      val obj = vt.pop.cast[vrt.Obj]
      ensureNonNull(vt, obj){
        vt.push(obj.members(index)._2().toStackVal)
      }
    }
  }
  case class PutField(index: Int) extends OpCode{
    def op(vt: VmThread) ={
      val (value, obj) = (vt.pop, vt.pop.cast[vrt.Obj])
      ensureNonNull(vt, obj){
        obj.members(index)._2() = value
      }
    }
  }
}
