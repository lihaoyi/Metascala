package metascala

import opcodes.OpCode
import rt.Thread


/**
 * `opcodes` contains the stack manipulating behavior of each individual
 * opcode. Each opcode is a case class or case object extending the trait
 * [[metascala.opcodes.OpCode]]. These are split into three separate files to help keep
 * compile times down.
 *
 * A large number of the opcodes are unused (they extend [[metascala.opcodes.UnusedOpCode]])
 * as ASM folds these into other opcodes for us automatically. For example,
 * `LDC`, `LDC_W` and `LDC_2W` all get folded into `LDC` by ASM before being
 * made available to Metascala. Furthermore, some opcodes are immediately
 * converted into optimized variants living in Optimized.scala, for example with
 * pre-computed method or field offsets.
 */
package object opcodes{

  object UnusedOpCode extends OpCode{
    def op(vt: Thread)  = ???
  }
  implicit def intToByte(n: Int) = n.toByte
  def throwNPE(vt: Thread) = {
    import vt.vm
    vt.throwException(vrt.Obj.allocate("java/lang/NullPointerException"))
  }
}
