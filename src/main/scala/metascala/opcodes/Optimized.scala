package metascala
package opcodes

import metascala.imm.{Type}
import metascala.vrt
import rt.{Thread, Method}

/**
 * The optimized versions of various opcodes; these hold direct references
 * or indexes to the class/method/field that they are referring to, and are
 * created the first time the un-optimized versions are run using the swapOpCode()
 * method. This saves us the cost of performing the search for the relevant
 * class/method/field every time.
 */
object Optimized {
  case class New(cls: rt.Cls) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm._
      vt.push(vrt.Obj(cls.name)(vt.vm).address)
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

  case class InvokeVirtual(vTableIndex: Int, argCount: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val args = vt.popArgs(argCount+1)


      val objCls =
        args.head match{
          case a if a.isObj => a.obj.cls
          case _ => vt.vm.ClsTable(imm.Type.Cls("java/lang/Object"))
        }
      try{
        val mRef = objCls.vTable(vTableIndex)
        vt.prepInvoke(mRef, args)
      }catch{case e: IndexOutOfBoundsException =>
        println("IndexOutOfBoundsException")
        println(args.head)
        println(objCls.name)
        println("Methods " + objCls.vTable.length)
        objCls.vTable.map{
          case Method.Cls(cls, _, method) =>
            cls.name + " " + method.name + method.desc.unparse
          case Method.Native(clsName, imm.Sig(name, desc), op) =>
            "Native " + name + desc.unparse
        }.foreach(println)
        throw e
      }


    }
  }

  case class GetStatic(field: rt.Var) extends OpCode{
    def op(vt: Thread) = vt.push(field())
  }
  case class PutStatic(field: rt.Var) extends OpCode{
    def op(vt: Thread) = field() = vt.pop
  }
  case class GetField(index: Int, size: Int) extends OpCode{

    def op(vt: Thread) = {
      import vt.vm
      val obj = vt.pop.obj
      for(i <- 0 until size){
        vt.push(obj.members(index))
      }
    }
  }
  case class PutField(index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val values = for(i <- 0 until size) yield vt.pop
      val obj = vt.pop.obj
      for(i <- 0 until size){
        obj.members(index + i) = values(i)
      }


    }
  }
}
