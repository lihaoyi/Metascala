package metascala

import metascala.natives.Bindings

import scala.collection.mutable

/**
  * Created by lihaoyi on 13/8/17.
  */
trait VMInterface extends VMInterface0 {
  val log: (=> String) => Unit
  def alloc[T](func: Registrar => T): T
  val arrayTypeCache: mutable.Buffer[imm.Type]

  def resolveDirectRef(owner: imm.Type.Cls, sig: imm.Sig): Option[rt.Method]
  val interned: mutable.Buffer[Ref]
  val typeObjCache: mutable.HashMap[imm.Type, Ref]
  def natives: Bindings

}

class Registrar(f: Ref => Unit, val vm: VMInterface) extends Function1[Ref, Unit]{
  def apply(i: Ref) = f(i)
}
