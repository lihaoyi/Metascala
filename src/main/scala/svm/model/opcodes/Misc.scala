package svm.model.opcodes

import svm.model.TypeDesc
import TypeDesc._

object Misc {
  case class Goto(label: Int) extends BaseOpCode(167, "goto"){
    def op = ctx => ctx.jumpTo(label)
  }

  case object Ret extends BaseOpCode(169, "ret"){ def op = ??? }
  case object Jsr extends BaseOpCode(168, "jsr"){ def op = ??? }
  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends BaseOpCode(170, "tableswitch"){
    def op = ctx => ctx.swapStack{ case Intish(top) :: rest =>
      val newPc: Int =
        if (targets.isDefinedAt(top - min))
          targets(top - min)
        else
          defaultTarget
      ctx.jumpTo(newPc)
      rest
    }
  }
  case class LookupSwitch(defaultTarget: Int, keys: Seq[Int], targets: Seq[Int]) extends BaseOpCode(171, "lookupswitch"){
    def op = ctx => ctx.swapStack{ case Intish(top) :: rest =>
      val newPc: Int = keys.zip(targets).toMap.get(top).getOrElse(defaultTarget: Int)
      ctx.jumpTo(newPc)
      rest
    }
  }

  case object IReturn extends BaseOpCode(172, "ireturn"){ def op = ctx => ctx.returnVal(Some(ctx.stack.head)) }
  case object LReturn extends BaseOpCode(173, "lreturn"){ def op = ctx => ctx.returnVal(Some(ctx.stack.head)) }
  case object FReturn extends BaseOpCode(174, "freturn"){ def op = ctx => ctx.returnVal(Some(ctx.stack.head)) }
  case object DReturn extends BaseOpCode(175, "dreturn"){ def op = ctx => ctx.returnVal(Some(ctx.stack.head)) }
  case object AReturn extends BaseOpCode(176, "areturn"){ def op = ctx => ctx.returnVal(Some(ctx.stack.head)) }
  case object Return extends BaseOpCode(177, "return"){ def op = ctx => ctx.returnVal(None) }

  case class GetStatic(owner: String, name: String, desc: String) extends BaseOpCode(178, "getstatic"){
    def op = ctx => ctx.frame.stack = ctx.classes(owner).statics(name) :: ctx.stack
  }
  case class PutStatic(owner: String, name: String, desc: String) extends BaseOpCode(179, "putstatic"){
    def op = ctx => ctx.swapStack{ case value :: stack =>
      ctx.classes(owner).statics(name) = value
      stack
    }
  }

  case class GetField(owner: String, name: String, desc: String) extends BaseOpCode(180, "getfield"){
    def op = _.swapStack{
      case (objectRef: svm.Object) :: stack => objectRef.members(name) :: stack
    }
  }
  case class PutField(owner: String, name: String, desc: String) extends BaseOpCode(181, "putfield"){
    def op = _.swapStack {
      case value :: (objectRef: svm.Object) :: stack =>
        objectRef.members(name) = value
        stack
    }
  }

  case class InvokeVirtual(owner: String, name: String, desc: String) extends BaseOpCode(182, "invokevirtual"){
    def op = ctx => {
      val argCount = TypeDesc.read(desc).args.length
      val (args, rest) = ctx.frame.stack.splitAt(argCount+1)
      val cls = args.last.asInstanceOf[svm.Object].cls
      val method = cls.method(name, desc).get


      ctx.frame.stack = rest
      ctx.prepInvoke(cls, method, args.reverse)
    }
  }
  case class InvokeSpecial(owner: String, name: String, desc: String) extends BaseOpCode(183, "invokespecial"){
    def op = ctx => {

      val argCount = TypeDesc.read(desc).args.length
      val cls = ctx.classes(owner)
      val method = cls.method(name, desc).get

      val (args, rest) = ctx.frame.stack.splitAt(argCount+1)
      ctx.frame.stack = rest
      ctx.prepInvoke(cls, method, args.reverse)
    }
  }
  case class InvokeStatic(owner: String, name: String, desc: String) extends BaseOpCode(184, "invokestatic"){

    def op = ctx => {

      val argCount = TypeDesc.read(desc).args.length

      val cls = ctx.classes(owner)
      val method = cls.method(name, desc).get

      val (args, rest) = ctx.frame.stack.splitAt(argCount)
      ctx.frame.stack = rest
      ctx.prepInvoke(cls, method, args.reverse)
    }
  }
  case class InvokeInterface(owner: String, name: String, desc: String) extends BaseOpCode(185, "invokeinterface"){ def op = ??? }

  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends BaseOpCode(186, "invokedynamic"){ def op = ??? }

  case class New(desc: String) extends BaseOpCode(187, "new"){
    def op = ctx => {
      ctx.frame.stack ::= new svm.Object(ctx.classes(desc), ctx.classes)
    }
  }
  case class NewArray(typeCode: Int) extends BaseOpCode(188, "newarray"){
    def op = _.swapStack { case Intish(count) :: stack =>
      val newArray = typeCode match{
        case 4 => new Array[Boolean](count)
        case 5 => new Array[Char](count)
        case 6 => new Array[Float](count)
        case 7 => new Array[Double](count)
        case 8 => new Array[Byte](count)
        case 9 => new Array[Short](count)
        case 10 => new Array[Int](count)
        case 11 => new Array[Long](count)
      }
      newArray :: stack
    }
  }
  case class ANewArray(desc: String) extends BaseOpCode(189, "anewarray"){
    def op = _.swapStack{
      case Intish(count) :: stack => new Array[Object](count) :: stack
    }
  }

  case object ArrayLength extends BaseOpCode(190, "arraylength"){
    def op = _.swapStack{
      case (array: Array[_]) :: stack => array.length :: stack
    }
  }

  case object AThrow extends BaseOpCode(191, "athrow"){
    def op = ctx => {
      val exception :: stack = ctx.stack
      ctx.throwException(exception)
    }
  }
  case class CheckCast(desc: String) extends BaseOpCode(192, "checkcast"){
    def op = ctx => {

    }
  }
  case class InstanceOf(desc: String) extends BaseOpCode(193, "instanceof"){
    def op = ctx => ctx.swapStack{ case top :: rest =>
      val res = top match{
        case null => 0
        case x: svm.Object => x.cls.isInstanceOf(desc, ctx.classes)
      }
      res :: ctx.frame.stack
    }
  }
  case object MonitorEnter extends BaseOpCode(194, "monitorenter"){ def op = ???  }
  case object MonitorExit extends BaseOpCode(195, "monitorexit"){ def op = ??? }

  // Not used, because ASM folds these into the following bytecode for us
  //===============================================================
  case object Wide extends UnusedOpCode(196, "wide")
  //===============================================================

  case class MultiANewArray(desc: String, dims: Int) extends BaseOpCode(197, "multianewarray"){
    def op = ctx => {

      val (dimValues, newStack) = ctx.stack.splitAt(dims)
      val dimArray = dimValues.map(x => x.asInstanceOf[Int])
      val array = java.lang.reflect.Array.newInstance(TypeDesc.fromChar(desc.last), dimArray:_*)
      ctx.frame.stack = array :: newStack
    }
  }

  case class IfNull(label: Int) extends BaseOpCode(198, "ifnull"){
    def op = ctx => ctx.swapStack{ case ref :: stack =>
      if (ref == null) ctx.jumpTo(label)
      stack
    }
  }

  case class IfNonNull(label: Int) extends BaseOpCode(199, "ifnonnull"){
    def op = ctx => ctx.swapStack{ case ref :: stack =>
      if (ref != null) ctx.jumpTo(label)
      stack
    }
  }

  // Not used, because ASM converts these to normal Goto()s and Jsr()s
  //===============================================================
  case object GotoW extends UnusedOpCode(200, "goto_w")
  case object JsrW extends UnusedOpCode(201, "jsr_w")
  //===============================================================

}
