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

  case class InvokeStatic(mRef: rt.MethodRef, argCount: Int) extends OpCode{
    def op(vt: VmThread) = {

      vt.prepInvoke(mRef, vt.popArgs(argCount))
    }
  }

  case class InvokeSpecial(mRef: rt.MethodRef, argCount: Int) extends OpCode{
    def op(vt: VmThread) = {

      vt.prepInvoke(mRef, vt.popArgs(argCount+1))
    }
  }

  case class InvokeVirtual(methodIndex: Int, argCount: Int) extends OpCode{
    def op(vt: VmThread) = {
      val args = vt.popArgs(argCount+1)
      ensureNonNull(vt, args.head){
        val objCls =
          args.head match{
            case a: vrt.Obj => a.cls
            case _ => vt.vm.Classes(imm.Type.Cls("java/lang/Object"))
          }
        val mRef = objCls.methodList(methodIndex)
        vt.prepInvoke(mRef, args)
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
