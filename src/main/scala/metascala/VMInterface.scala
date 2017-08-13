package metascala

import metascala.natives.Bindings
import metascala.rt.{Registrar, VMInterface0}

import scala.collection.mutable

/**
  * Created by lihaoyi on 13/8/17.
  */
trait VMInterface extends VMInterface0 {
  val log: (=> String) => Unit
  def alloc[T](func: Registrar => T): T
  val arrayTypeCache: mutable.Buffer[imm.Type]
  def arr(address: Int): rt.Arr
  def resolveDirectRef(owner: imm.Type.Cls, sig: imm.Sig): Option[rt.Method]
  val interned: mutable.Buffer[Ref]
  val typeObjCache: mutable.HashMap[imm.Type, Ref]


}


