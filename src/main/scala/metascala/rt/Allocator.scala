package metascala.rt

import metascala._
import metascala.imm
import metascala.util.{Constants, Ref}

import scala.collection.mutable

/**
  * Used to temporary hold newly allocated objects. This is necessary because
  * it often takes some initialization before the object is ready to be
  * placed into local variables or fields, during which time it is
  * "unreachable". Temporarily placing them in the registry avoids them
  * getting prematurely garbage collected during this period.
  */
class Allocator()(implicit val vm: rt.Obj.VMInterface) {
  private[this] val tempRegistry = new mutable.ArrayBuffer[Ref]()

  def register(r: Ref) = tempRegistry.append(r)
  def registered: TraversableOnce[Ref] = tempRegistry
  /**
    * Allocates and returns an array of the specified type, with `n` elements.
    * This is multiplied with the size of the type being allocated when
    * calculating the total amount of memory benig allocated
    */
  def newArr(t: imm.Type, n: scala.Int): Arr = {
    newArr(t, Array.fill[Int](n * t.size)(0).map(x => new Ref.ManualRef(x): Ref))
  }
  def newArr(innerType: imm.Type, backing0: TraversableOnce[Ref]): Arr = {
    val backing = backing0.toArray
    val address = vm.heap.allocate(Constants.arrayHeaderSize+ backing.length)(register)
    //    println("Allocating Array[" + innerType.name + "] at " + address)
    vm.heap(address()) = rt.Arr.packType(imm.Type.Arr(innerType))

    vm.heap(address() + 1) = backing.length / innerType.size
    backing.map(_()).copyToArray(vm.heap.memory, address() + Constants.arrayHeaderSize)
    new Arr(address)
  }
  def newObj(cls: rt.Cls, initMembers: (String, Ref)*): Obj = {
    val address = vm.heap.allocate(Constants.objectHeaderSize + cls.fieldList.length)(register)
    //    println("Allocating " + cls.name + " at " + address)
    vm.heap(address()) = -cls.index
    vm.heap(address() + 1) = cls.fieldList.length
    val obj = new Obj(address)
    for ((s, v) <- initMembers){
      obj(s) = v()
    }
    obj
  }

}


