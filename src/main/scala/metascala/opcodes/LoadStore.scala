package metascala
package opcodes

import org.objectweb.asm
import metascala.imm.{Type}

import metascala.vrt
import rt.Thread

object LoadStore {

  case object Nop extends OpCode{
    def op(vt: Thread) = ()
  }

  class PushOpCode[A](b: Prim[A])(value: A) extends OpCode{
    def op(vt: Thread) = b.write(value, vt.push)
  }

  case object AConstNull extends PushOpCode(I)(0)
  case object IConstNull extends PushOpCode(I)(-1)

  case object IConst0 extends PushOpCode(I)(0)
  case object IConst1 extends PushOpCode(I)(1)
  case object IConst2 extends PushOpCode(I)(2)
  case object IConst3 extends PushOpCode(I)(3)
  case object IConst4 extends PushOpCode(I)(4)
  case object IConst5 extends PushOpCode(I)(5)

  case object LConst0 extends PushOpCode(J)(0)
  case object LConst1 extends PushOpCode(J)(1)

  case object FConst0 extends PushOpCode(F)(0)
  case object FConst1 extends PushOpCode(F)(1)
  case object FConst2 extends PushOpCode(F)(2)

  case object DConst0 extends PushOpCode(D)(0)
  case object DConst1 extends PushOpCode(D)(1)

  class PushValOpCode(value: Int) extends OpCode{
    def op(vt: Thread) = vt.push(value)
  }

  case class BiPush(value: Int) extends PushValOpCode(value)
  case class SiPush(value: Int) extends PushValOpCode(value)


  case class Ldc(const: Any) extends OpCode{
    def op(vt: Thread) = {

      import vt.vm
      const match{
        case s: String => Virtualizer.pushVirtual(s, vt.push)
        case t: asm.Type => vt.push(vrt.Obj.allocate("java/lang/Class").address)
        case x: scala.Byte  => B.write(x, vt.push)
        case x: scala.Char  => C.write(x, vt.push)
        case x: scala.Short => S.write(x, vt.push)
        case x: scala.Int   => I.write(x, vt.push)
        case x: scala.Float => F.write(x, vt.push)
        case x: scala.Long  => J.write(x, vt.push)
        case x: scala.Double => D.write(x, vt.push)
      }


    }
  }

  // Not used, because ASM converts these Ldc(const: Any)
  //===============================================================
  val LdcW = UnusedOpCode
  val Ldc2W = UnusedOpCode
  //===============================================================

  abstract class Load[T](p: Prim[T]) extends OpCode{
    def op(vt: Thread) = {
      vt.pushFrom(vt.frame.locals, index, p.size)
    }
    def index: Int
  }

  case class ILoad(index: Int) extends Load(I)
  case class LLoad(index: Int) extends Load(J)
  case class FLoad(index: Int) extends Load(F)
  case class DLoad(index: Int) extends Load(D)
  case class ALoad(index: Int) extends Load(I)



  // Not used, because ASM converts these to raw XLoad(index: Int)s
  //===============================================================
  val ILoad0 = UnusedOpCode
  val ILoad1 = UnusedOpCode
  val ILoad2 = UnusedOpCode
  val ILoad3 = UnusedOpCode

  val LLoad0 = UnusedOpCode
  val LLoad1 = UnusedOpCode
  val LLoad2 = UnusedOpCode
  val LLoad3 = UnusedOpCode

  val FLoad0 = UnusedOpCode
  val FLoad1 = UnusedOpCode
  val FLoad2 = UnusedOpCode
  val FLoad3 = UnusedOpCode

  val DLoad0 = UnusedOpCode
  val DLoad1 = UnusedOpCode
  val DLoad2 = UnusedOpCode
  val DLoad3 = UnusedOpCode

  val ALoad0 = UnusedOpCode
  val ALoad1 = UnusedOpCode
  val ALoad2 = UnusedOpCode
  val ALoad3 = UnusedOpCode
  //===============================================================

  class LoadArray[T](p: Prim[T]) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val index = vt.pop
      val arr = vt.pop.arr
      checkBounds(index, arr, vt){
        vt.pushFrom(arr, index * p.size, p.size)
      }
    }
  }

  case object IALoad extends LoadArray(I)
  case object LALoad extends LoadArray(J)
  case object FALoad extends LoadArray(F)
  case object DALoad extends LoadArray(D)
  case object AALoad extends LoadArray(I)
  case object BALoad extends LoadArray(B)
  case object CALoad extends LoadArray(C)
  case object SALoad extends LoadArray(S)

  abstract class Store[T](p: Prim[T]) extends OpCode{
    def index: Int
    def op(vt: Thread) = vt.popTo(vt.frame.locals, index, p.size)
  }

  case class IStore(index: Int) extends Store(I)
  case class LStore(index: Int) extends Store(J )
  case class FStore(index: Int) extends Store(F)
  case class DStore(index: Int) extends Store(D)
  case class AStore(index: Int) extends Store(I)

  // Not used, because ASM converts these to raw XStore(index: Int)s
  //===============================================================
  val IStore0 = UnusedOpCode
  val IStore1 = UnusedOpCode
  val IStore2 = UnusedOpCode
  val IStore3 = UnusedOpCode

  val LStore0 = UnusedOpCode
  val LStore1 = UnusedOpCode
  val LStore2 = UnusedOpCode
  val LStore3 = UnusedOpCode

  val FStore0 = UnusedOpCode
  val FStore1 = UnusedOpCode
  val FStore2 = UnusedOpCode
  val FStore3 = UnusedOpCode

  val DStore0 = UnusedOpCode
  val DStore1 = UnusedOpCode
  val DStore2 = UnusedOpCode
  val DStore3 = UnusedOpCode

  val AStore0 = UnusedOpCode
  val AStore1 = UnusedOpCode
  val AStore2 = UnusedOpCode
  val AStore3 = UnusedOpCode
  //===============================================================

  def checkBounds(index: Int, arr: vrt.Arr, vt: Thread)(thunk: => Unit) = {
    import vt.vm
    if(0 <= index && index < arr.length){
      thunk
    }else{
      vt.throwException(vrt.Obj.allocate("java/lang/ArrayIndexOutOfBoundsException",
        "detailMessage" -> Virtualizer.pushVirtual(""+index).apply(0)
      ))
    }
  }
  class StoreArray[T](p: Prim[T]) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm

      val value = p.read(vt.pop)
      val index = vt.pop
      val arr = vt.pop.arr
      checkBounds(index, arr, vt){
        p.write(value, writer(arr, index * p.size))
      }
    }
  }

  case object IAStore extends StoreArray(I)
  case object LAStore extends StoreArray(J)
  case object FAStore extends StoreArray(F)
  case object DAStore extends StoreArray(D)
  case object AAStore extends StoreArray(I)
  case object BAStore extends StoreArray(B)
  case object CAStore extends StoreArray(C)
  case object SAStore extends StoreArray(S)
}
