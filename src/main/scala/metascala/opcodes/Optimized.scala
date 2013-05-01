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
      if (args.head == 0) vt.throwExWithTrace("java/lang/NullPointerException", "null")
      else vt.prepInvoke(mRef, args)
    }
  }

  case class InvokeVirtual(vTableIndex: Int, argCount: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val args = vt.popArgs(argCount)

      args.head match{
          case 0 => vt.throwExWithTrace("java/lang/NullPointerException", "null")
          case a =>
            val objCls: rt.Cls =
              if (a.isObj) a.obj.cls
              else "java/lang/Object"

            val mRef = objCls.vTable(vTableIndex)
            vt.prepInvoke(mRef, args)
        }
    }
  }

  case class GetStatic(cls: rt.Cls, index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      vt.pushFrom(cls.statics, index, size)
    }
  }
  case class PutStatic(cls: rt.Cls, index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      vt.popTo(cls.statics, index, size)
    }
  }

  case class GetField(index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val addr = vt.pop
      vm.log(vt.indent + "GETFIELD")
      vm.log(vt.indent + addr.obj.view)
      if (addr == 0) vt.throwExWithTrace("java/lang/NullPointerException", "null")
      else vt.pushFrom(addr.obj.members, index, size)
    }
  }
  case class PutField(index: Int, size: Int) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm

      val addr = vt.frame.stack(vt.frame.index - size - 1)
      if(addr == 0) vt.throwExWithTrace("java/lang/NullPointerException", "null")
      else {
        vt.popTo(addr.obj.members, index, size)
        vt.pop
      }
    }
  }
}
