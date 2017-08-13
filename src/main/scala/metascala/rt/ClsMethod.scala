package metascala.rt

import metascala.{Access, imm}
import metascala.opcodes.Code


/**
  * A reference to a method belonging to a class
  */
case class ClsMethod(clsIndex: Int,
                     methodIndex: Int,
                     sig: imm.Sig,
                     accessFlags: Int,
                     codeThunk: () => Code)extends Method{
  lazy val code = codeThunk()

  def static = (accessFlags & Access.Static) != 0
  def native = (accessFlags & Access.Native) != 0
}
