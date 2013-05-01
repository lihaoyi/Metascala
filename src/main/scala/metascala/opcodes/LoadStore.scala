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

  case class Const[A](override val toString: String)(b: Prim[A])(value: A) extends OpCode{
    def op(vt: Thread) = b.write(value, vt.push)
  }

  val AConstNull = Const("AConstNull")(I)(0)
  val IConstM1 = Const("IConstM1")(I)(-1)

  val IConst0 = Const("IConst0")(I)(0)
  val IConst1 = Const("IConst1")(I)(1)
  val IConst2 = Const("IConst2")(I)(2)
  val IConst3 = Const("IConst3")(I)(3)
  val IConst4 = Const("IConst4")(I)(4)
  val IConst5 = Const("IConst5")(I)(5)

  val LConst0 = Const("LConst0")(J)(0)
  val LConst1 = Const("LConst1")(J)(1)

  val FConst0 = Const("FConst0")(F)(0)
  val FConst1 = Const("FConst1")(F)(1)
  val FConst2 = Const("FConst2")(F)(2)

  val DConst0 = Const("DConst0")(D)(0)
  val DConst1 = Const("DConst1")(D)(1)

  case class Push(override val toString: String)(value: Int) extends OpCode{
    def op(vt: Thread) = vt.push(value)
  }
  val BiPush = Push("BiPush")(_: Int)
  val SiPush = Push("SiPush")(_: Int)


  case class Ldc(const: Any) extends OpCode{
    def op(vt: Thread) = {

      import vt.vm
      const match{
        case s: String =>
          val top = Virtualizer.pushVirtual(s).apply(0)
          vt.push(top)
        case t: asm.Type =>
          val clsObj = vrt.Obj.allocate("java/lang/Class",
            "name" -> Virtualizer.pushVirtual(t.getInternalName).apply(0)
          )
          vt.push(clsObj.address)
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


  case class Load[T](override val toString: String)(index: Int, p: Prim[T]) extends OpCode{
    def op(vt: Thread) = {
      vt.pushFrom(vt.frame.locals, index, p.size)
    }

  }

  val ILoad = Load("ILoad")(_: Int, I)
  val LLoad = Load("LLoad")(_: Int, J)
  val FLoad = Load("FLoad")(_: Int, F)
  val DLoad = Load("DLoad")(_: Int, D)
  val ALoad = Load("ALoad")(_: Int, I)



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

  case class LoadArray[T](override val toString: String)(p: Prim[T]) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val index = vt.pop
      val arr = vt.pop.arr
      checkBounds(index, arr, vt){
        vt.pushFrom(arr, index * p.size, p.size)
      }
    }
  }

  val IALoad = LoadArray("IALoad")(I)
  val LALoad = LoadArray("LALoad")(J)
  val FALoad = LoadArray("FALoad")(F)
  val DALoad = LoadArray("DALoad")(D)
  val AALoad = LoadArray("AALoad")(I)
  val BALoad = LoadArray("BALoad")(B)
  val CALoad = LoadArray("CALoad")(C)
  val SALoad = LoadArray("SALoad")(S)

  case class Store[T](index: Int, p: Prim[T], override val toString: String) extends OpCode{
    def op(vt: Thread) = vt.popTo(vt.frame.locals, index, p.size)
  }

  val IStore = Store(_: Int, I, "IStore")
  val LStore = Store(_: Int, J, "LStore")
  val FStore = Store(_: Int, F, "FStore")
  val DStore = Store(_: Int, D, "DStore")
  val AStore = Store(_: Int, I, "AStore")

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
  case class StoreArray[T](override val toString: String)(p: Prim[T]) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val top = vt.popArgs(p.size)
      val value = p.read(reader(top, 0))
      val index = vt.pop
      val arr = vt.pop.arr
      checkBounds(index, arr, vt){
        p.write(value, writer(arr, index * p.size))
      }
    }
  }

  val IAStore = StoreArray("IAStore")(I)
  val LAStore = StoreArray("LAStore")(J)
  val FAStore = StoreArray("FAStore")(F)
  val DAStore = StoreArray("DAStore")(D)
  val AAStore = StoreArray("AAStore")(I)
  val BAStore = StoreArray("BAStore")(B)
  val CAStore = StoreArray("CAStore")(C)
  val SAStore = StoreArray("SAStore")(S)
}
