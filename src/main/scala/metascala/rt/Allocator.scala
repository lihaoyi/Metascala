package metascala.rt

import metascala._
import metascala.imm
import metascala.util.{Constants, Ref, WritableRef}

import scala.collection.mutable

/**
  * Used to temporary hold newly allocated objects. This is necessary because
  * it often takes some initialization before the object is ready to be
  * placed into local variables or fields, during which time it is
  * "unreachable". Temporarily placing them in the registry avoids them
  * getting prematurely garbage collected during this period.
  */
class Allocator()(implicit val vm: rt.Obj.VMInterface) {
  private[this] val tempRegistry = new mutable.ArrayBuffer[WritableRef]()

  def obj(address: Int) = new rt.Obj(register(address))
  def arr(address: Int) = new rt.Arr(register(address))
  def register(address: Int): WritableRef = {
    val r = new Ref.UnsafeManual(address)
    tempRegistry.append(r)
    r
  }

  def registered: TraversableOnce[WritableRef] = tempRegistry

  /**
    * Allocates and returns an array of the specified type, with `n` elements.
    * This is multiplied with the size of the type being allocated when
    * calculating the total amount of memory benig allocated
    */
  def newArr(innerType: imm.Type, n: scala.Int): Arr = {
    val wordLength = innerType.size * n
    val address = newArr0(innerType, n)
    var i = address + Constants.arrayHeaderSize
    val target = address + Constants.arrayHeaderSize + wordLength
    while (i < target){
      vm.heap(i) = 0
      i += 1
    }
    new Arr(register(address))
  }

  def newArr0(innerType: imm.Type, n: scala.Int) = {
    val address = vm.heap.allocate(Constants.arrayHeaderSize + innerType.size * n)
    vm.heap(address) = rt.Arr.packType(imm.Type.Arr(innerType))

    vm.heap(address + 1) = n
    address
  }

  def newArr(innerType: imm.Type, backing0: TraversableOnce[Ref]): Arr = {
    val items = backing0.toArray
    val address = newArr0(innerType, items.length)
    items.map(_()).copyToArray(vm.heap.memory, address + Constants.arrayHeaderSize)
    new Arr(register(address))
  }

  def newObj(cls: imm.Type.Cls, initMembers: (String, Ref)*): Obj = {
    newObj0(vm.ClsTable(cls), initMembers:_*)
  }

  def newObj0(cls: rt.Cls, initMembers: (String, Ref)*): Obj = {
    val address = vm.heap.allocate(Constants.objectHeaderSize + cls.fieldList.length)
    //    println("Allocating " + cls.name + " at " + address)
    vm.heap(address) = -cls.index
    vm.heap(address + 1) = cls.fieldList.length
    val obj = new Obj(register(address))
    for ((s, v) <- initMembers){
      obj(s) = v()
    }
    obj
  }

}


