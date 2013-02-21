package svm.model
package opcodes




import svm.Frame
import collection.mutable
import scala.Some
import svm.model.a
import svm.model.Instruction._


object OpCodes {
  import TypeDesc._
  import OpCode._
  implicit def intToByte(n: I) = n.toByte

  val Nop = OpCode[Insn](0, "nop")((a, b) => ())
  case class PushOpCode(id: Byte, name: String, value: Any) extends OpCode[Insn]{
    def op = (ctx, _) => ctx.frame.stack ::= value
  }

  val AConstNull = PushOpCode(1, "aconst_null", null)
  val IConstNull = PushOpCode(2, "iconst_m1", -1)

  val IConst0 = PushOpCode(3, "iconst_0", 0)
  val IConst1 = PushOpCode(4, "iconst_1", 1)
  val IConst2 = PushOpCode(5, "iconst_2", 2)
  val IConst3 = PushOpCode(6, "iconst_3", 3)
  val IConst4 = PushOpCode(7, "iconst_4", 4)
  val IConst5 =  PushOpCode(8, "iconst_5", 5)

  val LConst0 = PushOpCode(9, "lconst_0", 0L)
  val LConst1 = PushOpCode(10, "lconst_1", 1L)

  val FConst0 = PushOpCode(11, "fconst_0", 0f)
  val FConst1 = PushOpCode(12, "fconst_1", 1f)
  val FConst2 = PushOpCode(13, "fconst_2", 2f)

  val DConst0 = PushOpCode(14, "dconst_0", 0d)
  val DConst1 = PushOpCode(15, "dconst_1", 1d)

  case class PushValOpCode(id: Byte, name: String) extends OpCode[IntInsn]{
    def op = (ctx, insn) => ctx.frame.stack ::= insn.operand
  }
  val BiPush = PushValOpCode(16, "bipush")
  val SiPush = PushValOpCode(17,"sipush")

  case class PushConstOpCode(id: Byte, name: String) extends OpCode[LdcInsn]{
    def op = (ctx, insn) => ctx.frame.stack ::= insn.cst
  }
  val Ldc = PushConstOpCode(18, "ldc")
  val LdcW = PushConstOpCode(19, "ldc_w")
  val Ldc2W = PushConstOpCode(20, "ldc2_w")

  case class PushLocalIndexed(id: Byte, name: String) extends OpCode[VarInsn]{
    def op = (ctx, insn) => ctx.frame.stack ::= ctx.frame.locals(insn.variable)
  }
  val ILoad = PushLocalIndexed(21, "iLoad")
  val LLoad = PushLocalIndexed(22, "lLoad")
  val FLoad = PushLocalIndexed(23, "fLoad")
  val DLoad = PushLocalIndexed(24, "dLoad")
  val ALoad = PushLocalIndexed(25, "aLoad")

  val ILoad0 = PushLocalIndexed(26, "iLoad_0")
  val ILoad1 = PushLocalIndexed(27, "iLoad_1")
  val ILoad2 = PushLocalIndexed(28, "iLoad_2")
  val ILoad3 = PushLocalIndexed(29, "iLoad_3")

  val LLoad0 = PushLocalIndexed(30, "lLoad_0")
  val LLoad1 = PushLocalIndexed(31, "lLoad_1")
  val LLoad2 = PushLocalIndexed(32, "lLoad_2")
  val LLoad3 = PushLocalIndexed(33, "lLoad_3")

  val FLoad0 = PushLocalIndexed(34, "fLoad_0")
  val FLoad1 = PushLocalIndexed(35, "fLoad_1")
  val FLoad2 = PushLocalIndexed(36, "fLoad_2")
  val FLoad3 = PushLocalIndexed(37, "fLoad_3")

  val DLoad0 = PushLocalIndexed(38, "dLoad_0")
  val DLoad1 = PushLocalIndexed(39, "dLoad_1")
  val DLoad2 = PushLocalIndexed(40, "dLoad_2")
  val DLoad3 = PushLocalIndexed(41, "dLoad_3")

  val ALoad0 = PushLocalIndexed(42, "aLoad_0")
  val ALoad1 = PushLocalIndexed(43, "aLoad_1")
  val ALoad2 = PushLocalIndexed(44, "aLoad_2")
  val ALoad3 = PushLocalIndexed(45, "aLoad_3")

  case class PushFromArray(id: Byte, name: String) extends OpCode[Insn]{
    def op = { (ctx, insn) =>
      val (index: Int) :: (array: Array[Any]) :: stack = ctx.frame.stack
      ctx.frame.stack = array(index) :: stack
    }
  }
  val IALoad = PushFromArray(46, "iaLoad")
  val LALoad = PushFromArray(47, "laLoad")
  val FALoad = PushFromArray(48, "faLoad")
  val DALoad = PushFromArray(49, "daLoad")
  val AALoad = PushFromArray(50, "aaLoad")
  val BALoad = PushFromArray(51, "baLoad")
  val CALoad = PushFromArray(52, "caLoad")
  val SALoad = PushFromArray(53, "saLoad")

  case class StoreLocal(id: Byte, name: String) extends OpCode[VarInsn]{
    def op = {(ctx, insn) =>
      val top :: stack = ctx.stack
      ctx.frame.locals(insn.variable) = top
      ctx.frame.stack = stack
    }
  }
  val IStore = StoreLocal(54, "istore")
  val LStore = StoreLocal(55, "lstore")
  val FStore = StoreLocal(56, "fstore")
  val DStore = StoreLocal(57, "dstore")
  val AStore = StoreLocal(58, "astore")

  val IStore0 = StoreLocal(59, "istore_0")
  val IStore1 = StoreLocal(60, "istore_1")
  val IStore2 = StoreLocal(61, "istore_2")
  val IStore3 = StoreLocal(62, "istore_3")

  val LStore0 = StoreLocal(63, "lstore_0")
  val LStore1 = StoreLocal(64, "lstore_1")
  val LStore2 = StoreLocal(65, "lstore_2")
  val LStore3 = StoreLocal(66, "lstore_3")

  val FStore0 = StoreLocal(67, "fstore_0")
  val FStore1 = StoreLocal(68, "fstore_1")
  val FStore2 = StoreLocal(69, "fstore_2")
  val FStore3 = StoreLocal(70, "fstore_3")

  val DStore0 = StoreLocal(71, "dstore_0")
  val DStore1 = StoreLocal(72, "dstore_1")
  val DStore2 = StoreLocal(73, "dstore_2")
  val DStore3 = StoreLocal(74, "dstore_3")

  val AStore0 = StoreLocal(75, "astore_0")
  val AStore1 = StoreLocal(76, "astore_1")
  val AStore2 = StoreLocal(77, "astore_2")
  val AStore3 = StoreLocal(78, "astore_3")

  case class StoreArray(id: Byte, name: String) extends OpCode[Insn]{
    def op = { (ctx, insn) =>
      val value :: (index: Int) :: (array: Array[Any]) :: stack = ctx.frame.stack
      array(index) = value
      ctx.frame.stack = stack
    }
  }
  val IAStore = StoreArray(79, "iastore")
  val LAStore = StoreArray(80, "lastore")
  val FAStore = StoreArray(81, "fastore")
  val DAStore = StoreArray(82, "dastore")
  val AAStore = StoreArray(83, "aastore")
  val BAStore = StoreArray(84, "bastore")
  val CAStore = StoreArray(85, "castore")
  val SAStore = StoreArray(86, "sastore")

  case class PureStackOpCode(id: Byte, name: String)(transform: List[Any] => List[Any]) extends OpCode[Insn]{
    def op = {(ctx, insn) =>
      ctx.frame.stack = transform(ctx.stack)
    }
  }
  val Pop = PureStackOpCode(87, "pop"){ case _ :: s => s }
  val Pop2 = PureStackOpCode(88, "pop2"){ case _ :: _ :: s => s }
  val Dup = PureStackOpCode(89, "dup"){ case top :: s => top :: top :: s }
  val DupX1 = PureStackOpCode(90, "dup_x1"){ case top :: x :: s => top :: x :: top :: s }
  val DupX2 = PureStackOpCode(91, "dup_x2"){ case top :: y :: x :: s => top :: y :: x :: top :: s }
  val Dup2 = PureStackOpCode(92, "dup2"){ case y :: x :: s => y :: x :: y :: x :: s }
  val Dup2X1 = PureStackOpCode(93, "dup2_x1"){ case a :: b :: x :: s => a :: b :: x :: a :: b :: s }
  val Dup2X2 = PureStackOpCode(94, "dup2_x2"){ case a :: b :: x :: y :: s => a :: b :: x :: y :: a :: b :: s }
  val Swap = PureStackOpCode(95, "swap"){ case x :: y :: s=> y :: x :: s }

  val IAdd = PureStackOpCode(96, "iadd"){ case (x: I) :: (y: I) :: s => (x + y) :: s }
  val LAdd = PureStackOpCode(97, "ladd"){ case (x: J) :: (y: J) :: s => (y + x) :: s}
  val FAdd = PureStackOpCode(98, "fadd"){ case (x: F) :: (y: F) :: s => (y + x) :: s}
  val DAdd = PureStackOpCode(99, "dadd"){ case (x: D) :: (y: D) :: s => (y + x) :: s}

  val ISub = PureStackOpCode(100, "isub"){ case (x: I) :: (y: I) :: s => (y - x) :: s}
  val LSub = PureStackOpCode(101, "lsub"){ case (x: J) :: (y: J) :: s => (y - x) :: s}
  val FSub = PureStackOpCode(102, "fsub"){ case (x: F) :: (y: F) :: s => (y - x) :: s}
  val DSub = PureStackOpCode(103, "dsub"){ case (x: D) :: (y: D) :: s => (y - x) :: s}

  val IMul = PureStackOpCode(104, "imul"){ case (x: I) :: (y: I) :: s => (y * x) :: s}
  val LMul = PureStackOpCode(105, "lmul"){ case (x: J) :: (y: J) :: s => (y * x) :: s}
  val FMul = PureStackOpCode(106, "fmul"){ case (x: F) :: (y: F) :: s => (y * x) :: s}
  val DMul = PureStackOpCode(107, "dmul"){ case (x: D) :: (y: D) :: s => (y * x) :: s}

  val IDiv = PureStackOpCode(108, "idiv"){ case (x: I) :: (y: I) :: s => (y / x) :: s}
  val LDiv = PureStackOpCode(109, "ldiv"){ case (x: J) :: (y: J) :: s => (y / x) :: s}
  val FDiv = PureStackOpCode(110, "fdiv"){ case (x: F) :: (y: F) :: s => (y / x) :: s}
  val DDiv = PureStackOpCode(111, "ddiv"){ case (x: D) :: (y: D) :: s => (y / x) :: s}

  val IRem = PureStackOpCode(112, "irem"){ case (x: I) :: (y: I) :: s => (y % x) :: s}
  val LRem = PureStackOpCode(113, "lrem"){ case (x: J) :: (y: J) :: s => (y % x) :: s}
  val FRem = PureStackOpCode(114, "frem"){ case (x: F) :: (y: F) :: s => (y % x) :: s}
  val DRem = PureStackOpCode(115, "drem"){ case (x: D) :: (y: D) :: s => (y % x) :: s}

  val INeg = PureStackOpCode(116, "ineg"){ case (x: I) :: s => -x :: s }
  val LNeg = PureStackOpCode(117, "lneg"){ case (x: J) :: s => -x :: s }
  val FNeg = PureStackOpCode(118, "fneg"){ case (x: F) :: s => -x :: s }
  val DNeg = PureStackOpCode(119, "dneg"){ case (x: D) :: s => -x :: s }

  val IShl = PureStackOpCode(120, "ishl"){ case (x: I) :: (y: I) :: s => (x << y) :: s }
  val LShl = PureStackOpCode(121, "lshl"){ case (x: J) :: (y: J) :: s => (x << y) :: s }
  val IShr = PureStackOpCode(122, "ishr"){ case (x: I) :: (y: I) :: s => (x >> y) :: s }
  val LShr = PureStackOpCode(123, "lshr"){ case (x: J) :: (y: J) :: s => (x >> y) :: s }

  val IUShr = PureStackOpCode(124, "iushr"){ case (x: I) :: (y: I) :: s => (x >>> y) :: s }
  val LUShr = PureStackOpCode(125, "lushr"){ case (x: J) :: (y: J) :: s => (x >>> y) :: s }

  val IAnd = PureStackOpCode(126, "iand"){ case s :+ (x: I) :+ (y: I) => s :+ (x & y) }
  val LAnd = PureStackOpCode(127, "land"){ case s :+ (x: J) :+ (y: I) => s :+ (x & y) }

  val IOr = PureStackOpCode(128, "ior"){ case s :+ (x: I) :+ (y: I) => s :+ (x | y) }
  val LOr = PureStackOpCode(129, "lor"){ case s :+ (x: J) :+ (y: I) => s :+ (x | y) }

  val IXOr = PureStackOpCode(130, "ixor"){ case s :+ (x: I) :+ (y: I) => s :+ (x ^ y) }
  val LXOr = PureStackOpCode(131, "lxor"){ case s :+ (x: J) :+ (y: I) => s :+ (x ^ y) }

  val IInc = OpCode[IIncInsn](132, "iinc"){
    case (ctx, insn) => ctx.frame.locals(insn.variable) = (ctx.frame.locals(insn.variable).asInstanceOf[Int]) + insn.incr
  }

  val I2L = PureStackOpCode(133, "i2l"){ case (x: I) :: s => x.toLong :: s}
  val I2F = PureStackOpCode(134, "i2f"){ case (x: I) :: s => x.toFloat :: s }
  val I2D = PureStackOpCode(135, "i2d"){ case (x: I) :: s => x.toDouble :: s }

  val L2I = PureStackOpCode(136, "l2i"){ case (x: J) :: s => x.toInt :: s }
  val L2F = PureStackOpCode(137, "l2f"){ case (x: J) :: s => x.toFloat :: s }
  val L2D = PureStackOpCode(138, "l2d"){ case (x: J) :: s => x.toDouble :: s }

  val F2I = PureStackOpCode(139, "f2i"){ case (x: F) :: s => x.toInt :: s }
  val F2L = PureStackOpCode(140, "f2l"){ case (x: F) :: s => x.toLong :: s }
  val F2D = PureStackOpCode(141, "f2d"){ case (x: F) :: s => x.toDouble :: s }

  val D2I = PureStackOpCode(142, "d2i"){ case (x: D) :: s => x.toInt :: s }
  val D2L = PureStackOpCode(143, "d2l"){ case (x: D) :: s => x.toLong :: s }
  val D2F = PureStackOpCode(144, "d2f"){ case (x: D) :: s  => x.toFloat :: s }

  val I2B = PureStackOpCode(145, "i2b"){ case (x: I) :: s => x.toByte :: s }
  val I2C = PureStackOpCode(146, "i2c"){ case (x: I) :: s => x.toChar :: s }
  val I2S = PureStackOpCode(147, "i2s"){ case (x: I) :: s => x.toShort :: s }

  val LCmp = PureStackOpCode(148, "lcmp"){ case (x: J) :: (y: J) :: s => x.compare(y) :: s }
  val FCmpl = PureStackOpCode(149, "fcmpl"){ case (x: F) :: (y: F) :: s => x.compare(y) :: s }
  val FCmpg = PureStackOpCode(150, "fcmpg"){ case (x: F) :: (y: F) :: s => x.compare(y) :: s }
  val DCmpl = PureStackOpCode(151, "dcmpl"){ case (x: D) :: (y: D) :: s => x.compare(y) :: s }
  val DCmpg = PureStackOpCode(152, "dcmpg"){ case (x: D) :: (y: D) :: s => x.compare(y) :: s }


  case class UnaryBranch(id: Byte, name: String)(pred: Int => Boolean) extends OpCode[JumpInsn]{
    def op = {(ctx, insn) =>
      val (top: Int) :: stack = ctx.stack
      if(pred(top)) ctx.jumpTo(insn.label)
    }
  }

  val IfEq = UnaryBranch(153, "ifeq")(_ == 0)
  val IfNe = UnaryBranch(154, "ifne")(_ != 0)
  val IfLt = UnaryBranch(155, "iflt")(_ < 0)
  val IfGe = UnaryBranch(156, "ifge")(_ >= 0)
  val IfGt = UnaryBranch(157, "ifgt")(_ > 0)
  val IfLe = UnaryBranch(158, "ifle")(_ <= 0)

  case class BinaryBranch(id: Byte, name: String)(pred: (Int, Int) => Boolean) extends OpCode[JumpInsn]{
    def op = {(ctx, insn) =>
      val (top: Int) :: (next: Int) :: stack = ctx.stack
      if(pred(top, next)) ???
    }
  }

  val IfICmpEq = BinaryBranch(159, "if_icmpeq")(_ == _)
  val IfICmpNe = BinaryBranch(160, "if_icmpne")(_ != _)
  val IfICmpLt = BinaryBranch(161, "if_icmplt")(_ < _)
  val IfICmpGe = BinaryBranch(162, "if_icmpge")(_ >= _)
  val IfICmpGt = BinaryBranch(163, "if_icmpgt")(_ > _)
  val IfICmpLe = BinaryBranch(164, "if_icmple")(_ <= _)
  val IfACmpEq = BinaryBranch(165, "if_acmpeq")(_ == _)
  val IfACmpNe = BinaryBranch(166, "if_acmpne")(_ != _)

  val Goto = OpCode[JumpInsn](167, "goto"){ (ctx, insn) => ctx.jumpTo(insn.label) }

  val Jsr = OpCode[JumpInsn](168, "jsr"){ (ctx, insn) => ??? }
  val Ret = OpCode[VarInsn](169, "ret"){ (ctx, insn) => ??? }
  val TableSwitch = OpCode[TableSwitchInsn](170, "tableswitch"){ (ctx, insn) => ??? }
  val LookupSwitch = OpCode[LookupSwitchInsn](171, "lookupswitch"){(ctx, insn) => ??? }

  val IReturn = OpCode[Insn](172, "ireturn"){ (ctx, _) => ctx.returnVal(Some(ctx.stack.head)) }
  val LReturn = OpCode[Insn](173, "lreturn"){ (ctx, _) => ctx.returnVal(Some(ctx.stack.head)) }
  val FReturn = OpCode[Insn](174, "freturn"){ (ctx, _) => ctx.returnVal(Some(ctx.stack.head)) }
  val DReturn = OpCode[Insn](175, "dreturn"){ (ctx, _) => ctx.returnVal(Some(ctx.stack.head)) }
  val AReturn = OpCode[Insn](176, "areturn"){ (ctx, _) => ctx.returnVal(Some(ctx.stack.head)) }
  val Return = OpCode[Insn](177, "return"){ (ctx, _) => ctx.returnVal(None) }

  val GetStatic = OpCode[FieldInsn](178, "getstatic"){case (ctx, insn) =>
    ctx.frame.stack = ctx.classes(insn.owner).statics(insn.name) :: ctx.stack
  }
  val PutStatic = OpCode[FieldInsn](179, "putstatic"){case (ctx, insn) =>
    val value :: stack = ctx.stack
    ctx.classes(insn.owner).statics(insn.name) = value
    ctx.frame.stack = stack
  }

  val GetField = OpCode[FieldInsn](180, "getfield"){case (ctx, insn) =>
    val (objectRef: svm.Object) :: stack = ctx.stack
    ctx.frame.stack = objectRef.members(insn.name) :: stack
  }
  val PutField = OpCode[FieldInsn](181, "putfield"){case (ctx, insn) =>
    val value :: (objectRef: svm.Object) :: stack = ctx.stack
    objectRef.members(insn.name) = value
    ctx.frame.stack = stack
  }

  val InvokeVirtual = OpCode[MethodInsn](182, "invokevirtual"){case (ctx, insn) => ???}
  val InvokeSpecial = OpCode[MethodInsn](183, "invokespecial"){case (ctx, insn) => ???}
  val InvokeStatic = OpCode[MethodInsn](184, "invokestatic"){case (ctx, insn) =>
    import ctx._

    val cls = classes(insn.owner)
    val method = cls.classFile
                    .methods
                    .find(_.name == insn.name)
                    .get

    thread.threadStack.push(new Frame(
      runningClass = cls,
      method = method,
      locals = mutable.Seq.fill(method.misc.maxLocals)(null)

    ))
    //new Array[Object](count) :: stack
  }
  val InvokeInterface = OpCode[MethodInsn](185, "invokeinterface"){case (ctx, insn) => ???}
  val InvokeDynamic = OpCode[InvokeDynamicInsn](186, "invokedynamic"){ case (ctx, insn) =>
    ???
  }

  val New = OpCode[TypeInsn](187, "new"){ (ctx, insn) => ??? }
  val NewArray = OpCode[IntInsn](188, "newarray"){case (ctx, insn) =>
    val (count: Int) :: stack = ctx.stack
    val newArray = insn.operand match{
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
  val ANewArray = OpCode[TypeInsn](189, "anewarray"){case (ctx, insn) =>
    val (count: Int) :: stack = ctx.stack
    ctx.frame.stack = new Array[Object](count) :: stack
  }

  val ArrayLength = PureStackOpCode(190, "arraylength"){case (array: Array[_]) :: stack =>
    array.length :: stack
  }
  val AThrow = OpCode[Insn](191, "athrow"){ (ctx, insn) =>
    val exception :: stack = ctx.stack
    ctx.throwException(exception)
  }
  val CheckCast = OpCode[TypeInsn](192, "checkcast"){ (ctx, insn) => ??? }
  val InstanceOf = OpCode[TypeInsn](193, "instanceof"){ (ctx, insn) => ??? }
  val MonitorEnter = OpCode[Insn](194, "monitorenter"){ (ctx, insn) =>
    val monitor :: stack = ctx.stack
    ctx.frame.stack = stack
    ???
  }
  val MonitorExit = OpCode[Insn](195, "monitorexit"){ (ctx, insn) =>
    val monitor :: stack = ctx.stack
    ctx.frame.stack = stack
    ???
  }
  val Wide = OpCode[Nothing](196, "wide"){case (ctx, insn) => ??? }
  val MultiANewArray = OpCode[MultiANewArrayInsn](197, "multianewarray"){case (ctx, insn) =>


    val (dims, newStack) = ctx.stack.splitAt(insn.dims)
    val dimArray = dims.map(x => x.asInstanceOf[Int])
    val array = java.lang.reflect.Array.newInstance(classOf[Object], dimArray:_*)
    ctx.frame.stack = array :: newStack
  }

  val IfNull = OpCode[JumpInsn](198, "ifnull"){case (ctx, insn) =>
    val ref :: stack = ctx.stack
    if (ref == null) ctx.jumpTo(insn.label)
    ctx.frame.stack = stack
  }
  val IfNonNull = OpCode[JumpInsn](199, "ifnonnull"){case (ctx, insn) =>
    val ref :: stack = ctx.stack
    if (ref != null) ctx.jumpTo(insn.label)
    ctx.frame.stack = stack
  }

  val GotoW = OpCode[Nothing](200, "goto_w"){case (ctx, insn) => ??? }
  val JsrW = OpCode[Nothing](201, "jsr_w"){case (ctx, insn) => ??? }

  def apply[T <: Instruction](n: Int): OpCode[T] = all((n + 256) % 256).asInstanceOf[OpCode[T]]
  val all = Seq(
    Nop,
    AConstNull,
    IConstNull,
    IConst0,
    IConst1,
    IConst2,
    IConst3,
    IConst4,
    IConst5,
    LConst0,
    LConst1,
    FConst0,
    FConst1,
    FConst2,
    DConst0,
    DConst1,
    BiPush,
    SiPush,
    Ldc,
    LdcW,
    Ldc2W,

    ILoad,
    LLoad,
    FLoad,
    DLoad,
    ALoad,

    ILoad0,
    ILoad1,
    ILoad2,
    ILoad3,

    LLoad0,
    LLoad1,
    LLoad2,
    LLoad3,

    FLoad0,
    FLoad1,
    FLoad2,
    FLoad3,

    DLoad0,
    DLoad1,
    DLoad2,
    DLoad3,

    ALoad0,
    ALoad1,
    ALoad2,
    ALoad3,

    IALoad,
    LALoad,
    FALoad,
    DALoad,
    AALoad,
    BALoad,
    CALoad,
    SALoad,

    IStore,
    LStore,
    FStore,
    DStore,
    AStore,

    IStore0,
    IStore1,
    IStore2,
    IStore3,

    LStore0,
    LStore1,
    LStore2,
    LStore3,

    FStore0,
    FStore1,
    FStore2,
    FStore3,

    DStore0,
    DStore1,
    DStore2,
    DStore3,

    AStore0,
    AStore1,
    AStore2,
    AStore3,

    IAStore,
    LAStore,
    FAStore,
    DAStore,
    AAStore,
    BAStore,
    CAStore,
    SAStore,

    Pop,
    Pop2,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    Swap,

    IAdd,
    LAdd,
    FAdd,
    DAdd,

    ISub,
    LSub,
    FSub,
    DSub,

    IMul,
    LMul,
    FMul,
    DMul,

    IDiv,
    LDiv,
    FDiv,
    DDiv,

    IRem,
    LRem,
    FRem,
    DRem,

    INeg,
    LNeg,
    FNeg,
    DNeg,

    IShl,
    LShl,
    IShr,
    LShr,

    IUShr,
    LUShr,

    IAnd,
    LAnd,

    IOr,
    LOr,

    IXOr,
    LXOr,

    IInc,

    I2L,
    I2F,
    I2D,

    L2I,
    L2F,
    L2D,
    F2I,
    F2L,
    F2D,

    D2I,
    D2L,
    D2F,

    I2B,
    I2C,
    I2S,

    LCmp,
    FCmpl,
    FCmpg,
    DCmpl,
    DCmpg,

    IfEq,
    IfNe,
    IfLt,
    IfGe,
    IfGt,
    IfLe,

    IfICmpEq,
    IfICmpNe,
    IfICmpLt,
    IfICmpGe,
    IfICmpGt,
    IfICmpLe,
    IfACmpEq,
    IfACmpNe,

    Goto,
    Jsr,
    Ret,
    TableSwitch,
    LookupSwitch,
    IReturn,
    LReturn,
    FReturn,
    DReturn,
    AReturn,
    Return,
    GetStatic,
    PutStatic,
    GetField,
    PutField,
    InvokeVirtual,
    InvokeSpecial,
    InvokeStatic,
    InvokeInterface,
    InvokeDynamic,

    New,
    NewArray,
    ANewArray,

    ArrayLength,
    AThrow,
    CheckCast,
    InstanceOf,
    MonitorEnter,
    MonitorExit,
    Wide,
    MultiANewArray,
    IfNull,
    IfNonNull,
    GotoW,
    JsrW
  )
}




