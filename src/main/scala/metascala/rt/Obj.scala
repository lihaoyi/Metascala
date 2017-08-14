package metascala.rt

import collection.mutable
import metascala._
import metascala.util.{Constants, Ref}


object Obj{
  trait VMInterface extends rt.Cls.VMInterface{

    def theUnsafe: rt.Obj
    def obj(address: Int): rt.Obj

    def isArr(address: Int): Boolean
    def isObj(address: Int): Boolean
  }


  implicit def unwrap1(x: Obj): Int = x.address.apply()
  implicit def unwrap2(x: Obj): Ref = x.address
}

class Obj(val address: Ref)
         (implicit vm: rt.Cls.VMInterface) {
  /**
   * Layout
   * ------
   * 0 Class Index
   * 1 Length
   * 2 Field0
   * 3 Field1
   * ...
   * N FieldN-2
   */
  import vm._

  def cls: rt.Cls = vm.ClsTable.clsIndex(-vm.heap(address()))

  object members extends mutable.Seq[Int]{
    def apply(n: Int): Int = {
      vm.heap(address() + n + Constants.objectHeaderSize)
    }

    def update(n: Int, v: Int): Unit = {
      vm.heap(address() + n + Constants.objectHeaderSize) = v
    }

    def length = cls.fieldList.length

    def iterator: Iterator[Int] = new Iterator[Int]{
      var index = 0
      def hasNext = index < members.length
      def next() = {
        val x = vm.heap(address() + index + Constants.objectHeaderSize)
        index += 1
        x
      }
    }
  }

  def tpe = cls.tpe

  def heapSize = cls.heapSize

  def apply(name: String): Int = {
    members(tpe.fieldList.lastIndexWhere(_.name == name))
  }

  def update(name: String, value: Int) = {

    val index = tpe.fieldList.lastIndexWhere(_.name == name)
    members(index + 1 - tpe.fieldList(index).desc.size) = value
  }

  def view = address + " " + vm.heap.memory.slice(address(), address() + heapSize).toList

  override def toString = {
    s"vrt.Obj(${cls.name})"
  }
}

