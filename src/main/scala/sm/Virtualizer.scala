package sm

import imm.Type
import virt.Obj

object Virtualizer {
  def fromVirtual(x: Any)(implicit vm: VM): Any = {
    def cloneArray[T](x: Array[T]): Array[T] = {
      val newArray = x.clone()
      for(i <- 0 until x.length){
        newArray(i) = fromVirtual(newArray(i)).cast[T]
      }
      newArray
    }
    val VArr = virt.Arr

    x match{
      case null => null
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Long => x
      case x: Float => x
      case x: Double => x
      case virt.Arr(imm.Type.Prim("Z"), backing) => cloneArray(backing)
      case virt.Arr(imm.Type.Prim("B"), backing) => cloneArray(backing)
      case virt.Arr(imm.Type.Prim("C"), backing) => cloneArray(backing)
      case virt.Arr(imm.Type.Prim("S"), backing) => cloneArray(backing)
      case virt.Arr(imm.Type.Prim("I"), backing) => cloneArray(backing)
      case virt.Arr(imm.Type.Prim("F"), backing) => cloneArray(backing)
      case virt.Arr(imm.Type.Prim("L"), backing) => cloneArray(backing)
      case virt.Arr(imm.Type.Prim("D"), backing) => cloneArray(backing)
      case virt.Arr(tpe, backing) => cloneArray(backing)
      case virt.Obj("java/lang/String", members) => new String(fromVirtual(members(0)("value")).cast[Array[Object]].map(_.cast[Char]))
      case virt.Obj("java/lang/Integer", members) => members(0)("value").cast[Int]
      case virt.Obj("java/lang/Double", members) => members(0)("value").cast[Double]
      case virt.Obj(cls, members) => vm.log("Unknown Object " + cls)
    }
  }


  def toVirtual(x: Any)(implicit vm: VM): Any = {
    def cloneArray[T](x: Array[T]): Array[Any] = {
      val newArray = new Array[Any](x.length)
      for(i <- 0 until x.length){
        newArray(i) = toVirtual(x(i))
      }
      newArray
    }

    x match{
      case null => null
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Long => x
      case x: Float => x
      case x: Double => x

      case x: Array[Boolean] => virt.Arr(imm.Type.Prim("Z"), cloneArray(x))
      case x: Array[Byte]    => virt.Arr(imm.Type.Prim("B"), cloneArray(x))
      case x: Array[Char]    => virt.Arr(imm.Type.Prim("C"), cloneArray(x))
      case x: Array[Short]   => virt.Arr(imm.Type.Prim("S"), cloneArray(x))
      case x: Array[Int]     => virt.Arr(imm.Type.Prim("I"), cloneArray(x))
      case x: Array[Long]    => virt.Arr(imm.Type.Prim("J"), cloneArray(x))
      case x: Array[Float]   => virt.Arr(imm.Type.Prim("F"), cloneArray(x))
      case x: Array[Double]  => virt.Arr(imm.Type.Prim("D"), cloneArray(x))
      case x: Array[Any]     => virt.Arr(imm.Type.Cls("java/lang/Object"), cloneArray(x))
      case x: String => virt.Obj("java/lang/String", "value" -> toVirtual(x.toCharArray))
    }
  }
}
