package metascala

import metascala.natives.DefaultBindings

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
  def threads: List[rt.Thread]
  def natives: DefaultBindings.type

}
