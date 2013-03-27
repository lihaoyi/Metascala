package sm
package imm
package opcodes

import sm.imm.Type
import sm.{vrt, VmThread}
import sm.vrt.Obj

object Misc {
  case class Goto(label: Int) extends OpCode{
    def op = vt => vt.frame.pc = label
  }

  // These guys are meant to be deprecated in java 6 and 7
  //===============================================================
  val Ret = UnusedOpCode
  val Jsr = UnusedOpCode
  //===============================================================

  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends OpCode{
    def op = vt => {
      val vrt.Int(top) = vt.pop
      val newPc: Int =
        if (targets.isDefinedAt(top - min)) targets(top - min)
        else defaultTarget
      vt.frame.pc = newPc
    }
  }
  case class LookupSwitch(defaultTarget: Int, keys: Seq[Int], targets: Seq[Int]) extends OpCode{
    def op = vt => {
      val vrt.Int(top) = vt.pop
      val newPc: Int = keys.zip(targets).toMap.get(top).getOrElse(defaultTarget: Int)
      vt.frame.pc = newPc
    }
  }

  case object IReturn extends OpCode{ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object LReturn extends OpCode{ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object FReturn extends OpCode{ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object DReturn extends OpCode{ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object AReturn extends OpCode{ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object Return extends OpCode{ def op = vt => vt.returnVal(None) }

  case class GetStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op = vt => {

      import vt.vm
      import vm._
      vt.push(owner.cls.apply(owner, name).toStackVal)
    }
  }
  case class PutStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op = vt => {
      import vt.vm
      import vm._
      owner.cls.update(owner, name, vt.pop)
    }
  }

  case class GetField(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op = vt => vt.pop match {
      case (objectRef: vrt.Obj) => vt.push(objectRef(owner, name).toStackVal)
      case vrt.Null =>
        import vt._
        vt.throwException(vrt.Obj("java/lang/NullPointerException"))
    }
  }
  case class PutField(owner: Type.Cls, name: String, desc: Type) extends OpCode{
    def op = vt => (vt.pop, vt.pop) match {
      case (value, objectRef: vrt.Obj) =>
        objectRef(owner, name) = value
      case (value, vrt.Null) =>
        import vt._
        vt.throwException(vrt.Obj("java/lang/NullPointerException"))
    }
  }

  def ensureNonNull(vt: VmThread, x: Any)(thunk: => Unit) = {
    import vt._
    if (x == vrt.Null){
      throwException(vrt.Obj("java/lang/NullPointerException"))
    }else {
      thunk
    }
  }

  case class InvokeVirtual(owner: Type.Entity, name: String, desc: Type.Desc) extends OpCode{
    def op = vt => {
      import vt.vm
      val argCount = desc.args.length
      val (args, rest) = vt.frame.stack.splitAt(argCount+1)
      ensureNonNull(vt, args.last){
        val objType =
          args.last match{
            case a: vrt.Obj => a.cls.clsData.tpe
            case _ => owner
          }



        vt.frame.stack = rest
        vt.prepInvoke(objType, name, desc, args.reverse)
      }
    }
  }
  case class InvokeSpecial(owner: Type.Cls, name: String, desc: Type.Desc) extends OpCode{
    def op = implicit vt => {
      val argCount = desc.args.length
      val (args, rest) = vt.frame.stack.splitAt(argCount+1)
      vt.frame.stack = rest
      vt.prepInvoke(owner, name, desc, args.reverse)
    }
  }
  case class InvokeStatic(owner: Type.Cls, name: String, desc: Type.Desc) extends OpCode{

    def op = vt => {
      val argCount = desc.args.length
      val (args, rest) = vt.frame.stack.splitAt(argCount)
      vt.frame.stack = rest
      vt.prepInvoke(owner, name, desc, args.reverse)
    }
  }
  case class InvokeInterface(owner: Type.Cls, name: String, desc: Type.Desc) extends OpCode{
    def op = InvokeVirtual(owner, name, desc).op
  }

  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends OpCode{ def op = ??? }

  case class New(desc: Type.Cls) extends OpCode{
    def op = implicit vt => desc match {
      case _ =>
        import vt.vm._
        vt.push(new vrt.Obj(desc)(vt.vm))
    }
  }
  case class NewArray(typeCode: Int) extends OpCode{
    def op = vt => {
      val vrt.Int(count) = vt.pop

      val newArray = typeCode match{
        case 4  => vrt.Arr.Prim[Boolean](count)
        case 5  => vrt.Arr.Prim[Char](count)
        case 6  => vrt.Arr.Prim[Float](count)
        case 7  => vrt.Arr.Prim[Double](count)
        case 8  => vrt.Arr.Prim[Byte](count)
        case 9  => vrt.Arr.Prim[Short](count)
        case 10 => vrt.Arr.Prim[Int](count)
        case 11 => vrt.Arr.Prim[Long](count)
      }
      vt.push(newArray)
    }
  }
  case class ANewArray(desc: imm.Type.Ref) extends OpCode{
    def op = vt => {
      val vrt.Int(count) = vt.pop
      vt.push(vrt.Arr.Obj(desc, count))
    }
  }

  case object ArrayLength extends OpCode{
    def op = vt => {
      vt.push(vt.pop.asInstanceOf[vrt.Arr].backing.length)
    }
  }

  case object AThrow extends OpCode{
    def op = vt => {
      vt.throwException(vt.pop.asInstanceOf[vrt.Obj])
    }
  }
  case class CheckCast(desc: Type.Entity) extends OpCode{
    def op = vt => {
      import vt._

      val top = vt.pop
      vt.push(top)
      top match{
        case vrt.Null => ()
        case vrt.Unit => ()
        case (top: vrt.Ref with vrt.StackVal) if !check(top.refType, desc) =>
          vt.throwException(
            vrt.Obj("java/lang/ClassCastException",
              "detailMessage" -> s"${top.refType.unparse} cannot be converted to ${desc.unparse}"
            )
          )
        case _ => ()
      }
    }
  }
  def check(s: imm.Type.Entity, t: imm.Type.Entity)(implicit vm: VM): Boolean = {
    (s, t) match{

      case (s: Type.Cls, t) => s.cls.checkIsInstanceOf(t)
      case (s: Type.Arr, Type.Cls("java/lang/Object")) => true
      case (s: Type.Arr, Type.Cls("java/lang/Cloneable")) => true
      case (s: Type.Arr, Type.Cls("java/io/Serializable")) => true
      case (Type.Arr(Type.Prim(a)), Type.Arr(Type.Prim(b))) => a == b
      case (Type.Arr(sc: Type.Entity), Type.Arr(tc: Type.Entity)) => check(sc, tc)
      case _ => false
    }
  }
  case class InstanceOf(desc: Type.Entity) extends OpCode{
    def op = implicit vt => {

      import vt._
      import vm._
      val res = vt.pop match{
        case vrt.Null => 0
        case obj: vrt.Ref => if (check(obj.refType, desc)) 1 else 0
      }

      vt.push(res)
    }
  }
  case object MonitorEnter extends OpCode{
    def op = _.pop
  }
  case object MonitorExit extends OpCode{
    def op = _.pop
  }

  // Not used, because ASM folds these into the following bytecode for us
  //===============================================================
  val Wide = UnusedOpCode
  //===============================================================

  case class MultiANewArray(desc: Type.Arr, dims: Int) extends OpCode{
    def op = vt => {
      def rec(dims: List[Int], tpe: Type.Entity): vrt.Arr = {

        (dims, tpe) match {
          case (size :: Nil, Type.Arr(Type.Prim(c))) =>
            imm.Type.Prim.Info.charMap(c).newVirtArray(size)

          case (size :: Nil, Type.Arr(innerType)) =>
            new vrt.Arr.Obj(innerType.cast[imm.Type.Ref], Array.fill[vrt.Val](size)(innerType.default))
          case (size :: tail, Type.Arr(innerType)) =>
            new vrt.Arr.Obj(innerType.cast[imm.Type.Ref], Array.fill[vrt.Val](size)(rec(tail, innerType)))
        }
      }
      val (dimValues, newStack) = vt.frame.stack.splitAt(dims)
      val dimArray = dimValues.map(x => x.asInstanceOf[vrt.Int].v).toList
      val array = rec(dimArray, desc)
      vt.push(array)
    }
  }

  case class IfNull(label: Int) extends OpCode{
    def op = vt => {
      if (vt.pop == vrt.Null) vt.frame.pc = label
    }
  }

  case class IfNonNull(label: Int) extends OpCode{
    def op = vt => {
      if (vt.pop != vrt.Null) vt.frame.pc = label
    }
  }

  // Not used, because ASM converts these to normal Goto()s and Jsr()s
  //===============================================================
  val GotoW = UnusedOpCode
  val JsrW = UnusedOpCode
  //===============================================================

}
