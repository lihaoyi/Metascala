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
      import vt.vm
      val obj = vrt.Obj.allocate(cls.name)
      vt.push(obj.address)
    }
  }

  case class InvokeStatic(mRef: rt.Method, argCount: Int) extends OpCode{
    def op(vt: Thread) = {
      vt.prepInvoke(mRef, vt.popArgs(argCount))
    }
  }

  case class InvokeSpecial(mRef: rt.Method, argCount: Int) extends OpCode{
    def op(vt: Thread) = {
      val args = vt.popArgs(argCount+1)
      if (args.head == 0) throwNPE(vt)
      else vt.prepInvoke(mRef, args)
    }
  }

  case class InvokeVirtual(vTableIndex: Int, argCount: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val args = vt.popArgs(argCount)

      args.head match{
          case 0 => throwNPE(vt)
          case a =>
            val objCls: rt.Cls =
              if (a.isObj)
                a.obj.cls
              else
                "java/lang/Object"

            val mRef = objCls.vTable(vTableIndex)
            vt.prepInvoke(mRef, args)
        }
    }
  }

  case class GetStatic(cls: rt.Cls, index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      for(i <- 0 until size) vt.push(cls.statics(index + i))
    }
  }
  case class PutStatic(cls: rt.Cls, index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      for(i <- 0 until size) cls.statics(index + i) = vt.pop
    }
  }
  case class GetField(index: Int, size: Int) extends OpCode{

    def op(vt: Thread) = {
      import vt.vm
      val addr = vt.pop
      if (addr == 0) throwNPE(vt)
      else
        for(i <- 0 until size){
          vt.push(addr.obj.members(index + i))
        }
    }
  }
  case class PutField(index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val values = for(i <- 0 until size) yield vt.pop
      val addr = vt.pop

      if(addr == 0) throwNPE(vt)
      else
        for(i <- 0 until size){
          addr.obj.members(index + i) = values(i)
        }
    }
  }
}
