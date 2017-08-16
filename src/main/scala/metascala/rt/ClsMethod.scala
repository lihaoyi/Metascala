package metascala.rt

import metascala.imm
import metascala.opcodes.Code
import metascala.util.Access


/**
  * A reference to a method belonging to a class
  */
case class ClsMethod(clsIndex: Int,
                     methodIndex: Int,
                     sig: imm.Sig,
                     accessFlags: Int,
                     codeThunk: () => Code) extends Method{
  lazy val code = codeThunk()

  def static = (accessFlags & Access.Static) != 0
  def native = (accessFlags & Access.Native) != 0

  def localsSize = code.localSize
}
