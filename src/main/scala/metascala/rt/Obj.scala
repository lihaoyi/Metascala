package metascala.rt

import collection.mutable
import metascala._
import metascala.imm
import metascala.util.{Constants, Ref}


object Obj{
  trait VMInterface extends rt.Cls.VMInterface{

    def theUnsafe: rt.Obj
    def obj(address: Int): rt.Obj

    def isArr(address: Int): Boolean
    def isObj(address: Int): Boolean
  }


  class Registrar(f: Ref => Unit, val vm: VMInterface) extends Function1[Ref, Unit]{
    def apply(i: Ref) = f(i)
  }

  /**
    * Allocates and returns an array of the specified type, with `n` elements.
    * This is multiplied with the size of the type being allocated when
    * calculating the total amount of memory benig allocated
    */
  def allocArr(t: imm.Type, n: scala.Int)(implicit registrar: Registrar): Arr = {
    allocArr(t, Array.fill[Int](n * t.size)(0).map(x => new Ref.ManualRef(x): Ref))
  }
  def allocArr(innerType: imm.Type, backing0: TraversableOnce[Ref])(implicit registrar: Registrar): Arr = {
    implicit val vm = registrar.vm
    val backing = backing0.toArray
    val address = vm.heap.allocate(Constants.arrayHeaderSize+ backing.length)(registrar.apply)
    //    println("Allocating Array[" + innerType.name + "] at " + address)
    vm.heap(address()) = vm.arrayTypeCache.length

    vm.arrayTypeCache.append(innerType)
    vm.heap(address() + 1) = backing.length / innerType.size
    backing.map(_()).copyToArray(vm.heap.memory, address() + Constants.arrayHeaderSize)

    new Arr(address)
  }
  def alloc(cls: rt.Cls, initMembers: (String, Ref)*)(implicit registrar: Registrar): Obj = {
    implicit val vm = registrar.vm
    val address = vm.heap.allocate(Constants.objectHeaderSize + cls.fieldList.length)(registrar.apply)
//    println("Allocating " + cls.name + " at " + address)
    vm.heap(address()) = -cls.index
    vm.heap(address() + 1) = cls.fieldList.length
    val obj = new Obj(address)
    for ((s, v) <- initMembers){
      obj(s) = v()
    }
    obj
  }

  implicit def unwrap1(x: Obj): Int = x.address.apply()
  implicit def unwrap2(x: Obj): Ref = x.address
  def unapply(x: Int)(implicit vm: rt.Cls.VMInterface) = new Obj(x)
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

  def cls: rt.Cls = vm.ClsTable.clsIndex(-vm.heap(address()).toInt)

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

