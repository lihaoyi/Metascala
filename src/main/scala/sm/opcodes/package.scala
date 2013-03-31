package sm

import opcodes.OpCode


/**
 * `opcodes` contains the stack manipulating behavior of each individual
 * opcode. Each opcode is a case class or case object extending the trait
 * [[OpCode]]. These are split into three separate files to help keep
 * compile times down.
 *
 * A large number of the opcodes are unused (they extend [[sm.imm.opcodes.UnusedOpCode]])
 * as ASM folds these into other opcodes for us automatically. For example,
 * `LDC`, `LDC_W` and `LDC_2W` all get folded into `LDC` before being given to us
 */
package object opcodes{

  type B = vrt.Byte
  type C = vrt.Char
  type I = vrt.Int
  type J = vrt.Long
  type F = vrt.Float
  type D = vrt.Double
  type S = vrt.Short
  type Z = vrt.Boolean
  type L = vrt.Obj

  private[opcodes] case class UnusedOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op(vt: VmThread)  = ???
  }
  implicit def intToByte(n: Int) = n.toByte
  implicit class poppable(val vt: VmThread) extends AnyVal{
    def pop = vt.frame.stack.pop()
    def push(x: vrt.StackVal) = vt.frame.stack.push(x)
    def popArgs(n: Int) = {
      val args = new Array[vrt.StackVal](n)
      var i = n-1
      while(i >= 0) {args(i) = vt.frame.stack.pop(); i-=1}
      args
    }
  }

  private[opcodes] final def ensureNonNull(vt: VmThread, x: Any)(thunk: => Unit) = {
    import vt._
    if (x == vrt.Null){
      throwException(vrt.Obj("java/lang/NullPointerException"))
    }else {
      thunk
    }
  }


}
