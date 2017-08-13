package metascala

import scala.collection.mutable

trait ClsTable extends(imm.Type.Cls => rt.Cls){
  val clsIndex: mutable.ArrayBuffer[rt.Cls]
}

/**
  * Created by lihaoyi on 13/8/17.
  */
trait VMInterface0 {

  implicit def ClsTable: ClsTable

  def arrayTypeCache: mutable.Buffer[imm.Type]

  def theUnsafe: rt.Obj
  def heap: Heap
  def obj(address: Int): rt.Obj
  def arr(address: Int): rt.Arr
  def isArr(address: Int): Boolean
  def isObj(address: Int): Boolean
}
