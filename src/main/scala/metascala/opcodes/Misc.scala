package metascala
package opcodes
import metascala.imm.Type
import collection.mutable
import rt.Thread

object Misc {
  case class Goto(label: Int) extends OpCode{
    def op(vt: Thread) = vt.frame.pc = label
  }

  // These guys are meant to be deprecated in java 6 and 7
  //===============================================================
  val Ret = UnusedOpCode
  val Jsr = UnusedOpCode
  //===============================================================

  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends OpCode{
    def op(vt: Thread) =  {
      val top = vt.pop
      val newPc: Int =
        if (targets.isDefinedAt(top - min)) targets(top - min)
        else defaultTarget
      vt.frame.pc = newPc
    }
  }

  case class LookupSwitch(defaultTarget: Int, keys: Seq[Int], targets: Seq[Int]) extends OpCode{
    def op(vt: Thread) =  {
      val top = vt.pop
      val newPc: Int = keys.zip(targets).toMap.get(top).getOrElse(defaultTarget: Int)
      vt.frame.pc = newPc
    }
  }
  case class ReturnVal(n: Int) extends OpCode{
    def op(vt: Thread) =  vt.returnVal(n)
  }

  val IReturn = ReturnVal(1)
  val LReturn = ReturnVal(2)
  val FReturn = ReturnVal(1)
  val DReturn = ReturnVal(2)
  val AReturn = ReturnVal(1)
  val Return = ReturnVal(0)

  case class GetStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm

      val index = owner.cls.staticList.indexWhere(_.name == name)
      val size = owner.cls.staticList(index).desc.size
      Optimized.GetStatic(owner.cls, index, size)
    }

  }
  case class PutStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm
      val index = owner.cls.staticList.indexWhere(_.name == name)

      val size = owner.cls.staticList(index).desc.size
      Optimized.PutStatic(owner.cls, index, size)
    }
  }

  case class GetField(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm
      val index = owner.cls.fieldList.lastIndexWhere(_.name == name)
      val size = owner.cls.fieldList(index).desc.size
      Optimized.GetField(index - size + 1, size)
    }
  }
  case class PutField(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm
      val index = owner.cls.fieldList.lastIndexWhere(_.name == name)
      val size = owner.cls.fieldList(index).desc.size
      Optimized.PutField(index - size + 1, size)
    }
  }

  case class InvokeVirtual(owner: Type.Ref, sig: imm.Sig) extends OpCode{
    def op(vt: Thread) = {
      import vt.vm
      val index =
        owner
          .methodType
          .cls
          .vTable
          .indexWhere{ _.sig == sig }
      val x = sig.desc.argSize
      vt.swapOpCode(Optimized.InvokeVirtual(index, x + 1))
    }

  }



  case class InvokeSpecial(owner: Type.Cls, sig: imm.Sig) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm

      vm.resolveDirectRef(owner, sig) match{
        case None => StackManip.Pop
        case Some(methodRef) => Optimized.InvokeSpecial(methodRef, sig.desc.argSize)
      }

    }

  }

  case class InvokeStatic(owner: Type.Cls, sig: imm.Sig) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode {
      import vt.vm
      vm.resolveDirectRef(owner, sig) match{
        case Some(methodRef) => Optimized.InvokeStatic(methodRef, sig.desc.argSize)
      }
    }

  }

  case class InvokeInterface(owner: Type.Cls, sig: imm.Sig) extends OpCode{

    def op(vt: Thread) =  {
      import vt.vm
      val argCount = sig.desc.argSize
      val args = vt.popArgs(argCount + 1)

      val objType = args.head.obj.tpe
      val cls = vm.ClsTable(objType)
      vt.prepInvoke(
        cls.vTableMap(sig),
        args
      )

    }
  }

  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends OpCode{ def op(vt: Thread) = ??? }

  case class New(desc: Type.Cls) extends OpCode{
    def op(vt: Thread) = {
      vt.swapOpCode(
        Optimized.New(vt.vm.ClsTable(desc))
      )
    }
  }

  case class NewArray(typeCode: Int) extends OpCode{
    def op(vt: Thread) =  {
      val count = vt.pop
      import vt.vm
      val tpeChar = typeCode match{
        case 4  => 'Z'
        case 5  => 'C'
        case 6  => 'F'
        case 7  => 'D'
        case 8  => 'B'
        case 9  => 'S'
        case 10 => 'I'
        case 11 => 'J'
      }
      val newArray = vrt.Arr.allocate(imm.Type.Prim(tpeChar), count)
      vt.push(newArray.address)
    }
  }
  case class ANewArray(desc: imm.Type.Ref) extends OpCode{
    def op(vt: Thread) =  {
      val count = vt.pop
      import vt.vm
      vt.push(vrt.Arr.allocate(desc, count).address)
    }
  }

  case object ArrayLength extends OpCode{
    def op(vt: Thread) =  {
      import vt.vm
      val addr = vt.pop
      if (addr == 0) vt.throwExWithTrace("java/lang/NullPointerException", "null")
      else vt.push(addr.arr.length)
    }
  }

  case object AThrow extends OpCode{
    def op(vt: Thread) =  {
      import vt.vm
      vt.throwException(vt.pop.obj)
    }
  }
  case class CheckCast(desc: Type) extends OpCode{
    def op(vt: Thread) =  {
      import vt._

      val top = vt.pop
      vt.push(top)
      top match{
        case 0 => ()
        case top if (top.isArr && !check(top.arr.tpe, desc)) || (top.isObj && !check(top.obj.tpe, desc)) =>
          vt.throwExWithTrace("java/lang/ClassCastException", "")
        case _ => ()
      }
    }
  }
  def check(s: imm.Type, t: imm.Type)(implicit vm: VM): Boolean = {

    (s, t) match{

      case (s: Type.Cls, t: Type.Cls) => s.cls.typeAncestry.contains(t)
      case (s: Type.Arr, Type.Cls("java/lang/Object")) => true
      case (s: Type.Arr, Type.Cls("java/lang/Cloneable")) => true
      case (s: Type.Arr, Type.Cls("java/io/Serializable")) => true
      case (Type.Arr(Type.Prim(a)), Type.Arr(Type.Prim(b))) => a == b
      case (Type.Arr(sc: Type), Type.Arr(tc: Type)) => check(sc, tc)
      case _ => false
    }
  }
  case class InstanceOf(desc: Type) extends OpCode{
    def op(vt: Thread) = {

      import vt._
      import vm._
      val res = vt.pop match{
        case 0 => 0
        case top => if ((top.isArr && check(top.arr.tpe, desc)) || (top.isObj && check(top.obj.tpe, desc))) 1 else 0
      }

      vt.push(res)
    }
  }
  case object MonitorEnter extends OpCode{
    def op(vt: Thread) = vt.pop
  }
  case object MonitorExit extends OpCode{
    def op(vt: Thread) = vt.pop
  }

  // Not used, because ASM folds these into the following bytecode for us
  //===============================================================
  val Wide = UnusedOpCode
  //===============================================================

  case class MultiANewArray(desc: Type.Arr, dims: Int) extends OpCode{
    def op(vt: Thread) =  {
      import vt.vm
      def rec(dims: List[Int], tpe: Type): Val = {

        (dims, tpe) match {
          case (size :: tail, Type.Arr(innerType: imm.Type.Ref)) =>
            val newArr = vrt.Arr.allocate(innerType, size)
            for(i <- 0 until size){
              newArr(i) = rec(tail, innerType)
            }
            newArr.address

          case (size :: Nil, Type.Arr(innerType)) =>
            vrt.Arr.allocate(innerType, size).address
        }
      }
      val dimValues = vt.popArgs(dims).toList

      val array = rec(dimValues, desc)
      vt.push(array)
    }
  }

  val IfNull = StackManip.UnaryBranch("IfNull")(_: Int, _ == 0)
  val IfNonNull = StackManip.UnaryBranch("IfNull")(_: Int, _ != 0)

  // Not used, because ASM converts these to normal Goto()s and Jsr()s
  //===============================================================
  val GotoW = UnusedOpCode
  val JsrW = UnusedOpCode
  //===============================================================

}
