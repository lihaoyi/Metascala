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

  case class Const[A](b: Prim[A])(value: A)(name: String) extends OpCode{
    def op(vt: Thread) = b.write(value, vt.push)
    override def toString = s"$name($value)"
  }

  val AConstNull = Const(I)(0)("AConstNull")
  val IConstM1 = Const(I)(-1)("IConstM1")

  val IConst0 = Const(I)(0)("IConst0")
  val IConst1 = Const(I)(1)("IConst1")
  val IConst2 = Const(I)(2)("IConst2")
  val IConst3 = Const(I)(3)("IConst3")
  val IConst4 = Const(I)(4)("IConst4")
  val IConst5 = Const(I)(5)("IConst5")

  val LConst0 = Const(J)(0)("LConst0")
  val LConst1 = Const(J)(1)("LConst1")

  val FConst0 = Const(F)(0)("FConst0")
  val FConst1 = Const(F)(1)("FConst1")
  val FConst2 = Const(F)(2)("FConst2")

  val DConst0 = Const(D)(0)("DConst0")
  val DConst1 = Const(D)(1)("DConst1")

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
          val top = s.toVirtObj
          vt.push(top)
        case t: asm.Type =>
          val clsObj = vrt.Obj.allocate("java/lang/Class",
            "name" -> t.getInternalName.toVirtObj
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


  case class Load[T](index: Int, p: Prim[T])(name: String) extends OpCode{
    def op(vt: Thread) = {
      vt.pushFrom(vt.frame.locals, index, p.size)
    }
    override def toString = s"$name($index)"

  }

  val ILoad = Load(_: Int, I)("ILoad")
  val LLoad = Load(_: Int, J)("LLoad")
  val FLoad = Load(_: Int, F)("FLoad")
  val DLoad = Load(_: Int, D)("DLoad")
  val ALoad = Load(_: Int, I)("ALoad")



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

  case class LoadArray[T](p: Prim[T])(override val toString: String) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val index = vt.pop
      val arr = vt.pop.arr
      checkBounds(index, arr, vt){
        vt.pushFrom(arr, index * p.size, p.size)
      }
    }
  }

  val IALoad = LoadArray(I)("IALoad")
  val LALoad = LoadArray(J)("LALoad")
  val FALoad = LoadArray(F)("FALoad")
  val DALoad = LoadArray(D)("DALoad")
  val AALoad = LoadArray(I)("AALoad")
  val BALoad = LoadArray(B)("BALoad")
  val CALoad = LoadArray(C)("CALoad")
  val SALoad = LoadArray(S)("SALoad")

  case class Store[T](index: Int, p: Prim[T])(name: String) extends OpCode{
    def op(vt: Thread) = vt.popTo(vt.frame.locals, index, p.size)
    override def toString = s"$name($index)"
  }

  val IStore = Store(_: Int, I)("IStore")
  val LStore = Store(_: Int, J)("LStore")
  val FStore = Store(_: Int, F)("FStore")
  val DStore = Store(_: Int, D)("DStore")
  val AStore = Store(_: Int, I)("AStore")

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
        "detailMessage" -> (""+index).toVirtObj
      ))
    }
  }
  case class StoreArray[T](p: Prim[T])(override val toString: String) extends OpCode{
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

  val IAStore = StoreArray(I)("IAStore")
  val LAStore = StoreArray(J)("LAStore")
  val FAStore = StoreArray(F)("FAStore")
  val DAStore = StoreArray(D)("DAStore")
  val AAStore = StoreArray(I)("AAStore")
  val BAStore = StoreArray(B)("BAStore")
  val CAStore = StoreArray(C)("CAStore")
  val SAStore = StoreArray(S)("SAStore")
}
