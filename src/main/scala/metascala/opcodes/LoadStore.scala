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
    def op(vt: Thread) = b.push(value, vt.push)
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
  case object FConst1 extends PushOpCode(F)(0)
  case object FConst2 extends PushOpCode(F)(0)

  case object DConst0 extends PushOpCode(D)(0)
  case object DConst1 extends PushOpCode(D)(0)

  class PushValOpCode(value: Int) extends OpCode{
    def op(vt: Thread) = vt.frame.stack.push(value)
  }

  case class BiPush(value: Int) extends PushValOpCode(value)
  case class SiPush(value: Int) extends PushValOpCode(value)


  case class Ldc(const: Any) extends OpCode{
    def op(vt: Thread) = {

      import vt.vm
      import vm._
      const match{
        case s: String => vt.pushVirtual(s, vt.push)
        case t: asm.Type => vt.push(vrt.Obj.allocate("java/lang/Class").address)
        case x: scala.Byte  => B.push(x, vt.push)
        case x: scala.Char  => C.push(x, vt.push)
        case x: scala.Short => S.push(x, vt.push)
        case x: scala.Int   => I.push(x, vt.push)
        case x: scala.Float => F.push(x, vt.push)
        case x: scala.Long  => J.push(x, vt.push)
        case x: scala.Double => D.push(x, vt.push)
      }


    }
  }

  // Not used, because ASM converts these Ldc(const: Any)
  //===============================================================
  val LdcW = UnusedOpCode
  val Ldc2W = UnusedOpCode
  //===============================================================

  abstract class Load(size: Int) extends OpCode{
    def op(vt: Thread) = for (i <- (size-1) to 0 by -1){
      vt.push(vt.frame.locals(index + i))
    }
    def index: Int
  }

  case class ILoad(index: Int) extends Load(1)
  case class LLoad(index: Int) extends Load(2)
  case class FLoad(index: Int) extends Load(1)
  case class DLoad(index: Int) extends Load(2)
  case class ALoad(index: Int) extends Load(1)



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


  class PushFromArray() extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val index = vt.pop
      val arr = vt.pop.arr
      vt.push(arr(index))

    }
  }

  case object IALoad extends PushFromArray()
  case object LALoad extends PushFromArray()
  case object FALoad extends PushFromArray()
  case object DALoad extends PushFromArray()
  case object AALoad extends PushFromArray()
  case object BALoad extends PushFromArray()
  case object CALoad extends PushFromArray()
  case object SALoad extends PushFromArray()

  abstract class Store(size: Int) extends OpCode{
    def index: Int
    def op(vt: Thread) = for (i <- 0 until size){
      vt.frame.locals(index + i) = vt.pop
    }
  }
  case class IStore(index: Int) extends Store(1)
  case class LStore(index: Int) extends Store(2)
  case class FStore(index: Int) extends Store(1)
  case class DStore(index: Int) extends Store(2)
  case class AStore(index: Int) extends Store(1)

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

  class StoreArray[T](p: Prim[T]) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val value = p.pop(vt.pop)
      val index = vt.pop
      val arr = vt.pop.arr
      var i = 0
      p.push(value, { x =>
        arr(index + i) = x
        i += 1
      })
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
