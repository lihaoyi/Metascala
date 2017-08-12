package metascala

import metascala.natives.Bindings

import scala.collection.mutable

/**
  * Created by lihaoyi on 13/8/17.
  */
trait VMInterface {
  val log: (=> String) => Unit
  def heap: Heap
  def alloc[T](func: Registrar => T): T
  val arrayTypeCache: mutable.Buffer[imm.Type]
  implicit val ClsTable: (imm.Type.Cls => rt.Cls){
    val clsIndex: mutable.ArrayBuffer[rt.Cls]
  }
  def resolveDirectRef(owner: imm.Type.Cls, sig: imm.Sig): Option[rt.Method]
  val interned: mutable.Buffer[Ref]
  val typeObjCache: mutable.HashMap[imm.Type, Ref]
  def threads: List[rt.Thread]
  def natives: Bindings.type

}
