
package metascala.rt

import metascala.imm
import metascala.natives.Bindings



case class NativeMethod(cls: imm.Type.Cls,
                        sig: imm.Sig,
                        static: Boolean,
                        func: (Bindings.Interface, () => Int, Int => Unit) => Unit)
  extends Method{
  override def toString = s"Method.Native(${cls.javaName}, ${sig.unparse}})"

  def argSize = sig.desc.args.foldLeft(0)(_ + _.size)
  def localsSize = if (static) argSize else argSize + 1
}

