package svm.model.opcodes

import svm.model.Type
import svm.VmThread

object Misc {
  case class Goto(label: Int) extends BaseOpCode(167, "goto"){
    def op = vt => vt.frame.pc = label
  }

  // These guys are meant to be deprecated in java 6 and 7
  //===============================================================
  val Ret = UnusedOpCode(169, "ret")
  val Jsr = UnusedOpCode(168, "jsr")
  //===============================================================

  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends BaseOpCode(170, "tableswitch"){
    def op = vt => vt.swapStack{ case Intish(top) :: rest =>
      val newPc: Int =
        if (targets.isDefinedAt(top - min)) targets(top - min)
        else defaultTarget
      vt.frame.pc = newPc
      rest
    }
  }
  case class LookupSwitch(defaultTarget: Int, keys: Seq[Int], targets: Seq[Int]) extends BaseOpCode(171, "lookupswitch"){
    def op = vt => vt.swapStack{ case Intish(top) :: rest =>
      val newPc: Int = keys.zip(targets).toMap.get(top).getOrElse(defaultTarget: Int)
      vt.frame.pc = newPc
      rest
    }
  }

  case object IReturn extends BaseOpCode(172, "ireturn"){ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object LReturn extends BaseOpCode(173, "lreturn"){ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object FReturn extends BaseOpCode(174, "freturn"){ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object DReturn extends BaseOpCode(175, "dreturn"){ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object AReturn extends BaseOpCode(176, "areturn"){ def op = vt => vt.returnVal(Some(vt.frame.stack.head)) }
  case object Return extends BaseOpCode(177, "return"){ def op = vt => vt.returnVal(None) }

  case class GetStatic(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(178, "getstatic"){
    def op = vt => {
      import vt.vm
      import vm._
      vt.frame.stack = owner.cls.apply(owner, name) :: vt.frame.stack
    }
  }
  case class PutStatic(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(179, "putstatic"){
    def op = vt => vt.swapStack{ case value :: stack =>
      import vt.vm
      import vm._
      owner.cls.update(owner, name, value)
      stack
    }
  }

  case class GetField(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(180, "getfield"){
    def op = implicit vt => vt.swapStack{
      case (objectRef: svm.Obj) :: stack => ext(objectRef(owner, name)) :: stack
      case null :: rest =>
        import vt._
        vt.throwException(svm.Obj("java/lang/NullPointerException"))
        rest
    }
  }
  case class PutField(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(181, "putfield"){
    def op = implicit vt => vt.swapStack {
      case value :: (objectRef: svm.Obj) :: stack =>
        objectRef(owner, name) = value
        stack
      case value :: null :: rest =>
        import vt._
        vt.throwException(svm.Obj("java/lang/NullPointerException"))
        rest
    }
  }

  def ensureNonNull(vt: VmThread, x: Any)(thunk: => Unit) = {
    import vt._
    if (x == null){
      throwException(svm.Obj("java/lang/NullPointerException"))
    }else {
      thunk
    }
  }

  case class InvokeVirtual(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(182, "invokevirtual"){
    def op = vt => {
      import vt.vm
      val argCount = desc.args.length
      val (args, rest) = vt.frame.stack.splitAt(argCount+1)
      ensureNonNull(vt, args.last){

        vt.frame.stack = rest
        vt.prepInvoke(owner, name, desc, args.reverse)
      }
    }
  }
  case class InvokeSpecial(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(183, "invokespecial"){
    def op = implicit vt => {
      val argCount = desc.args.length
      val (args, rest) = vt.frame.stack.splitAt(argCount+1)
      vt.frame.stack = rest
      vt.prepInvoke(owner, name, desc, args.reverse)
    }
  }
  case class InvokeStatic(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(184, "invokestatic"){

    def op = vt => {
      val argCount = desc.args.length
      val (args, rest) = vt.frame.stack.splitAt(argCount)
      vt.frame.stack = rest
      vt.prepInvoke(owner, name, desc, args.reverse)
    }
  }
  case class InvokeInterface(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(185, "invokeinterface"){
    def op = InvokeVirtual(owner, name, desc).op
  }

  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends BaseOpCode(186, "invokedynamic"){ def op = ??? }

  case class New(desc: Type.Cls) extends BaseOpCode(187, "new"){
    def op = implicit vt => desc match {
      case _ =>
        import vt.vm._
        vt.frame.stack ::= new svm.Obj(desc)(vt.vm)
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
  case class ANewArray(desc: Type.Cls) extends BaseOpCode(189, "anewarray"){
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
    def op = vt => {
      val (exception: svm.Obj) :: stack = vt.frame.stack
      vt.frame.stack = stack
      vt.throwException(exception)

    }
  }
  case class CheckCast(desc: Type) extends BaseOpCode(192, "checkcast"){
    def op = vt => {

    }
  }
  case class InstanceOf(desc: Type) extends BaseOpCode(193, "instanceof"){
    def op = implicit vt => vt.swapStack{ case top :: rest =>
      import vt._
      val res = top match{
        case null => 0
        case x: svm.Obj =>  if(x.cls.checkIsInstanceOf(desc)) 1 else 0
        case x: Array[Object] => 1


      }
      res :: vt.frame.stack
    }
  }
  case object MonitorEnter extends BaseOpCode(194, "monitorenter"){
    def op = _.swapStack{case x :: stack => stack}
  }
  case object MonitorExit extends BaseOpCode(195, "monitorexit"){
    def op = _.swapStack{case x :: stack => stack}
  }

  // Not used, because ASM folds these into the following bytecode for us
  //===============================================================
  val Wide = UnusedOpCode(196, "wide")
  //===============================================================

  case class MultiANewArray(desc: Type.Arr, dims: Int) extends BaseOpCode(197, "multianewarray"){
    def op = vt => {
      def next(t: Type): Type =  t match {
        case Type.Arr(in) => next(in)
        case x => x
      }
      val (dimValues, newStack) = vt.frame.stack.splitAt(dims)
      val dimArray = dimValues.map(x => x.asInstanceOf[Int])
      val array = java.lang.reflect.Array.newInstance(next(desc).realCls, dimArray:_*)
      vt.frame.stack = array :: newStack
    }
  }

  case class IfNull(label: Int) extends BaseOpCode(198, "ifnull"){
    def op = vt => vt.swapStack{ case ref :: stack =>
      if (ref == null) vt.frame.pc = label
      stack
    }
  }

  case class IfNonNull(label: Int) extends BaseOpCode(199, "ifnonnull"){
    def op = vt => vt.swapStack{ case ref :: stack =>
      if (ref != null) vt.frame.pc = label
      stack
    }
  }

  // Not used, because ASM converts these to normal Goto()s and Jsr()s
  //===============================================================
  val GotoW = UnusedOpCode(200, "goto_w")
  val JsrW = UnusedOpCode(201, "jsr_w")
  //===============================================================

}
