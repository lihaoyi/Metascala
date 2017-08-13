package metascala

import scala.collection.mutable

/**
  * Created by lihaoyi on 13/8/17.
  */
trait VMInterface_2 {
  def heap: Heap
  def arrayTypeCache: mutable.Buffer[imm.Type]
}
