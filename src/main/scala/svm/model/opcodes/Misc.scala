package svm.model.opcodes

import svm.model.Context

import svm.model.Type.Primitives._
import svm.model.Type

object Misc {
  case class Goto(label: Int) extends BaseOpCode(167, "goto"){
    def op = ctx => ctx.jumpTo(label)
  }

  // These guys are meant to be deprecated in java 6 and 7
  //===============================================================
  val Ret = UnusedOpCode(169, "ret")
  val Jsr = UnusedOpCode(168, "jsr")
  //===============================================================

  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends BaseOpCode(170, "tableswitch"){
    def op = ctx => ctx.swapStack{ case Intish(top) :: rest =>
      val newPc: Int =
        if (targets.isDefinedAt(top - min)) targets(top - min)
        else defaultTarget
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

  case class GetStatic(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(178, "getstatic"){
    def op = implicit ctx => ctx.frame.stack = owner(owner, name) :: ctx.stack
  }
  case class PutStatic(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(179, "putstatic"){
    def op = implicit ctx => ctx.swapStack{ case value :: stack =>
      owner(owner, name) = value
      stack
    }
  }

  case class GetField(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(180, "getfield"){
    def op = implicit ctx => ctx.swapStack{
      case (objectRef: svm.Obj) :: stack => ext(objectRef(owner, name)) :: stack
      case null :: rest =>
        import ctx._
        ctx.throwException(svm.Obj("java/lang/NullPointerException"))
        rest
    }
  }
  case class PutField(owner: Type.Cls, name: String, desc: Type) extends BaseOpCode(181, "putfield"){
    def op = implicit ctx => ctx.swapStack {
      case value :: (objectRef: svm.Obj) :: stack =>
        objectRef(owner, name) = value
        stack
      case value :: null :: rest =>
        import ctx._
        ctx.throwException(svm.Obj("java/lang/NullPointerException"))
        rest
    }
  }

  def ensureNonNull(x: Any)(thunk: => Unit)(implicit ctx: Context) = {
    import ctx._
    if (x == null){
      ctx.throwException(svm.Obj("java/lang/NullPointerException"))
    }else {
      thunk
    }
  }

  case class InvokeVirtual(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(182, "invokevirtual"){
    def op = implicit ctx => {
      val argCount = desc.args.length
      val (args, rest) = ctx.frame.stack.splitAt(argCount+1)
      ensureNonNull(args.last){
        val cls = args.last match {
          case o: svm.Obj => o.cls
          case a: Array[_] => ctx(Type.Cls("java/lang/Object"))
        }
        /*svm.VM.log("InvokeVirtual")
        svm.VM.log(cls.name + " " + name + " " + desc)
        svm.VM.log("")
        cls.ancestry.flatMap(_.methods)
                    .map(""+_)
                    .foreach(svm.VM.log)
        svm.VM.log("")*/
        val method = cls.method(name, desc).get
        ctx.frame.stack = rest
        ctx.prepInvoke(cls, method, args.reverse)
      }
    }
  }
  case class InvokeSpecial(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(183, "invokespecial"){
    def op = implicit ctx => {

      val argCount = desc.args.length
      val cls = ctx(owner)
      val method = cls.method(name, desc).get

      val (args, rest) = ctx.frame.stack.splitAt(argCount+1)
      ctx.frame.stack = rest
      ctx.prepInvoke(cls, method, args.reverse)
    }
  }
  case class InvokeStatic(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(184, "invokestatic"){

    def op = ctx => {

      val argCount = desc.args.length

      val cls = ctx(owner)
      val method = cls.method(name, desc).get

      val (args, rest) = ctx.frame.stack.splitAt(argCount)
      ctx.frame.stack = rest
      ctx.prepInvoke(cls, method, args.reverse)
    }
  }
  case class InvokeInterface(owner: Type.Cls, name: String, desc: Type.Desc) extends BaseOpCode(185, "invokeinterface"){
    def op = InvokeVirtual(owner, name, desc).op
  }

  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends BaseOpCode(186, "invokedynamic"){ def op = ??? }

  case class New(desc: Type.Cls) extends BaseOpCode(187, "new"){
    def op = implicit ctx => desc match {
      case _ => ctx.frame.stack ::= new svm.Obj(desc)(ctx.vm)
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
    def op = ctx => {
      val (exception: svm.Obj) :: stack = ctx.stack
      ctx.frame.stack = stack
      ctx.throwException(exception)

    }
  }
  case class CheckCast(desc: Type) extends BaseOpCode(192, "checkcast"){
    def op = ctx => {

    }
  }
  case class InstanceOf(desc: Type) extends BaseOpCode(193, "instanceof"){
    def op = implicit ctx => ctx.swapStack{ case top :: rest =>
      import ctx._
      val res = top match{
        case null => 0
        case x: svm.Obj =>  if(x.cls.checkIsInstanceOf(desc)) 1 else 0
        case x: Array[Object] => 1


      }
      res :: ctx.frame.stack
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
    def op = ctx => {
      def next(t: Type): Type =  t match {
        case Type.Arr(in) => next(in)
        case x => x
      }
      val (dimValues, newStack) = ctx.stack.splitAt(dims)
      val dimArray = dimValues.map(x => x.asInstanceOf[Int])
      val array = java.lang.reflect.Array.newInstance(next(desc).realCls, dimArray:_*)
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
  val GotoW = UnusedOpCode(200, "goto_w")
  val JsrW = UnusedOpCode(201, "jsr_w")
  //===============================================================

}
