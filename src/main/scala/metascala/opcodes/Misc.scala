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

  case object IReturn extends OpCode{ def op(vt: Thread) =  vt.returnVal(1) }
  case object LReturn extends OpCode{ def op(vt: Thread) =  vt.returnVal(2) }
  case object FReturn extends OpCode{ def op(vt: Thread) =  vt.returnVal(1) }
  case object DReturn extends OpCode{ def op(vt: Thread) =  vt.returnVal(2) }
  case object AReturn extends OpCode{ def op(vt: Thread) =  vt.returnVal(1) }
  case object Return extends OpCode{ def op(vt: Thread) =  vt.returnVal(0) }

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
      println("PUTTING STATIC")
      import vt.vm
      val index = owner.cls.staticList.indexWhere(_.name == name)
      val size = owner.cls.staticList(index).desc.size
      Optimized.PutStatic(owner.cls, index, size)
    }

  }

  case class GetField(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm
      val index = owner.cls.fieldList.indexWhere(_.name == name)
      val size = owner.cls.fieldList(index).desc.size
      Optimized.GetField(index, size)
    }
  }
  case class PutField(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm
      val index = owner.cls.fieldList.indexWhere(_.name == name)
      val size = owner.cls.fieldList(index).desc.size
      Optimized.PutField(index, size)
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

      vt.swapOpCode(Optimized.InvokeVirtual(index, sig.desc.args.length))
    }

  }



  case class InvokeSpecial(owner: Type.Cls, sig: imm.Sig) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode{
      import vt.vm

      vm.resolveDirectRef(owner, sig) match{
        case None => StackManip.Pop
        case Some(methodRef) => Optimized.InvokeSpecial(methodRef, sig.desc.args.length)
      }

    }

  }

  case class InvokeStatic(owner: Type.Cls, sig: imm.Sig) extends OpCode{
    def op(vt: Thread) = vt.swapOpCode {
      import vt.vm
      vm.resolveDirectRef(owner, sig) match{
        case Some(methodRef) => Optimized.InvokeStatic(methodRef, sig.desc.args.length)
      }
    }

  }

  case class InvokeInterface(owner: Type.Cls, sig: imm.Sig) extends OpCode{

    def op(vt: Thread) =  {
      import vt.vm
      val argCount = sig.desc.args.length
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
        case 11 => 'L'
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
      vt.push(vt.pop.arr.length)
    }
  }

  case object AThrow extends OpCode{
    def op(vt: Thread) =  {
      ???
      //vt.throwException(vt.pop.asInstanceOf[vrt.Obj])
    }
  }
  case class CheckCast(desc: Type) extends OpCode{
    def op(vt: Thread) =  {
      import vt._

      val top = vt.pop
      vt.push(top)
      /*top match{
        case vrt.Null => ()
        case (top: vrt.Ref with vrt.StackVal) if !check(top.tpe, desc) =>
          vt.throwException(
            vrt.Obj("java/lang/ClassCastException",
              "detailMessage" -> vrt.virtString(s"${top.tpe.unparse} cannot be converted to ${desc.unparse}")
            )
          )
        case _ => ()
      }*/
      ???
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
      ???
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
      ???
    }
  }

  case class IfNull(label: Int) extends OpCode{
    def op(vt: Thread) =  {
      if (vt.pop == 0) vt.frame.pc = label
    }
  }

  case class IfNonNull(label: Int) extends OpCode{
    def op(vt: Thread) =  {
      if (vt.pop != 0) vt.frame.pc = label
    }
  }

  // Not used, because ASM converts these to normal Goto()s and Jsr()s
  //===============================================================
  val GotoW = UnusedOpCode
  val JsrW = UnusedOpCode
  //===============================================================

}
