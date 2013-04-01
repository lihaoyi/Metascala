package sm
package opcodes

import sm.imm.{Type}
import sm.vrt
import rt.{Thread, Method}

object Optimized {
  case class New(clsId: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm._
      vt.push(new vrt.Obj(vt.vm.ClsTable.clsIndex(clsId))(vt.vm))
    }
  }

  case class InvokeStatic(mRef: rt.Method, argCount: Int) extends OpCode{
    def op(vt: Thread) = {
      vt.prepInvoke(mRef, vt.popArgs(argCount))
    }
  }

  case class InvokeSpecial(mRef: rt.Method, argCount: Int) extends OpCode{
    def op(vt: Thread) = {

      vt.prepInvoke(mRef, vt.popArgs(argCount+1))
    }
  }

  case class InvokeVirtual(methodIndex: Int, argCount: Int) extends OpCode{
    def op(vt: Thread) = {
      val args = vt.popArgs(argCount+1)
      ensureNonNull(vt, args.head){

        val objCls =
          args.head match{
            case a: vrt.Obj => a.cls
            case _ => vt.vm.ClsTable(imm.Type.Cls("java/lang/Object"))
          }
        try{
          val mRef = objCls.methodList(methodIndex)
          vt.prepInvoke(mRef, args)
        }catch{case e: IndexOutOfBoundsException =>
          println("IndexOutOfBoundsException")
          println(args.head)
          println(objCls.name)
          println("Methods " + objCls.methodList.length)
          objCls.methodList.map{
            case Method.Cls(clsIndex, methodIndex, method) =>
              val cls = vt.vm.ClsTable.clsIndex(clsIndex)
              cls.name + " " + method.name + method.desc.unparse
            case Method.Native(nativeIndex) =>
              val (name, desc) = vt.vm.natives.trappedIndex(nativeIndex)._1
              "Native " + name + desc.unparse
          }.foreach(println)
          throw e
        }

      }
    }
  }

  case class GetStatic(field: rt.Var) extends OpCode{
    def op(vt: Thread) = vt.push(field().toStackVal)
  }
  case class PutStatic(field: rt.Var) extends OpCode{
    def op(vt: Thread) = field() = vt.pop
  }
  case class GetField(index: Int) extends OpCode{
    def op(vt: Thread) = {
      val obj = vt.pop.cast[vrt.Obj]
      ensureNonNull(vt, obj){
        vt.push(obj.members(index)._2().toStackVal)
      }
    }
  }
  case class PutField(index: Int) extends OpCode{
    def op(vt: Thread) ={
      val (value, obj) = (vt.pop, vt.pop.cast[vrt.Obj])
      ensureNonNull(vt, obj){
        obj.members(index)._2() = value
      }
    }
  }
}
