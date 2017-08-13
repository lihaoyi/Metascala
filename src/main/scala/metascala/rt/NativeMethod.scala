
package metascala.rt

import metascala.imm
import metascala.natives.Bindings



case class NativeMethod(clsName: String,
                        sig: imm.Sig,
                        func: (Bindings.Interface, () => Int, Int => Unit) => Unit)
  extends Method{
  override def toString = s"Method.Native(${clsName}, ${sig.unparse}})"
}

