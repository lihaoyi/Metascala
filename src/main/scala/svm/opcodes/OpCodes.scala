package svm.model
package opcodes


import svm.model.ConstantInfo
import ConstantInfo._
import svm.model.ConstantInfo
import svm.Frame
import collection.mutable


object OpCodes {
  import TypeDesc._
  import OpCodeGen._
  implicit def intToByte(n: I) = n.toByte

  val Nop = OpCode(0, "nop")()
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

  val BiPush = PushValOpCode(16, "bipush", ctx => ctx.nextByte().toInt)
  val SiPush = PushValOpCode(17,"sipush", ctx => ((ctx.nextByte() << 8) + ctx.nextByte()).toInt)

  val Ldc = PushConstOpCode(18, "ldc", ctx => ctx.nextByte())
  val LdcW = PushConstOpCode(19, "ldc_w", ctx => (ctx.nextByte() << 8) + ctx.nextByte())
  val Ldc2W = PushConstOpCode(20, "ldc2_w", ctx => (ctx.nextByte() << 8) + ctx.nextByte())

  val ILoad = PushLocalIndexed(21, "iLoad", -1)
  val LLoad = PushLocalIndexed(22, "lLoad", -1)
  val FLoad = PushLocalIndexed(23, "fLoad", -1)
  val DLoad = PushLocalIndexed(24, "dLoad", -1)
  val ALoad = PushLocalIndexed(25, "aLoad", -1)

  val ILoad0 = PushLocalIndexed(26, "iLoad_0", 0)
  val ILoad1 = PushLocalIndexed(27, "iLoad_1", 1)
  val ILoad2 = PushLocalIndexed(28, "iLoad_2", 2)
  val ILoad3 = PushLocalIndexed(29, "iLoad_3", 3)

  val LLoad0 = PushLocalIndexed(30, "lLoad_0", 0)
  val LLoad1 = PushLocalIndexed(31, "lLoad_1", 1)
  val LLoad2 = PushLocalIndexed(32, "lLoad_2", 2)
  val LLoad3 = PushLocalIndexed(33, "lLoad_3", 3)

  val FLoad0 = PushLocalIndexed(34, "fLoad_0", 0)
  val FLoad1 = PushLocalIndexed(35, "fLoad_1", 1)
  val FLoad2 = PushLocalIndexed(36, "fLoad_2", 2)
  val FLoad3 = PushLocalIndexed(37, "fLoad_3", 3)

  val DLoad0 = PushLocalIndexed(38, "dLoad_0", 0)
  val DLoad1 = PushLocalIndexed(39, "dLoad_1", 1)
  val DLoad2 = PushLocalIndexed(40, "dLoad_2", 2)
  val DLoad3 = PushLocalIndexed(41, "dLoad_3", 3)

  val ALoad0 = PushLocalIndexed(42, "aLoad_0", 0)
  val ALoad1 = PushLocalIndexed(43, "aLoad_1", 1)
  val ALoad2 = PushLocalIndexed(44, "aLoad_2", 2)
  val ALoad3 = PushLocalIndexed(45, "aLoad_3", 3)

  val IALoad = PushFromArray(46, "iaLoad")
  val LALoad = PushFromArray(47, "laLoad")
  val FALoad = PushFromArray(48, "faLoad")
  val DALoad = PushFromArray(49, "daLoad")
  val AALoad = PushFromArray(50, "aaLoad")
  val BALoad = PushFromArray(51, "baLoad")
  val CALoad = PushFromArray(52, "caLoad")
  val SALoad = PushFromArray(53, "saLoad")

  val IStore = StoreLocal(54, "istore", -1)
  val LStore = StoreLocal(55, "lstore", -1)
  val FStore = StoreLocal(56, "fstore", -1)
  val DStore = StoreLocal(57, "dstore", -1)
  val AStore = StoreLocal(58, "astore", -1)

  val IStore0 = StoreLocal(59, "istore_0", 0)
  val IStore1 = StoreLocal(60, "istore_1", 1)
  val IStore2 = StoreLocal(61, "istore_2", 2)
  val IStore3 = StoreLocal(62, "istore_3", 3)

  val LStore0 = StoreLocal(63, "lstore_0", 0)
  val LStore1 = StoreLocal(64, "lstore_1", 1)
  val LStore2 = StoreLocal(65, "lstore_2", 2)
  val LStore3 = StoreLocal(66, "lstore_3", 3)

  val FStore0 = StoreLocal(67, "fstore_0", 0)
  val FStore1 = StoreLocal(68, "fstore_1", 1)
  val FStore2 = StoreLocal(69, "fstore_2", 2)
  val FStore3 = StoreLocal(70, "fstore_3", 3)

  val DStore0 = StoreLocal(71, "dstore_0", 0)
  val DStore1 = StoreLocal(72, "dstore_1", 1)
  val DStore2 = StoreLocal(73, "dstore_2", 2)
  val DStore3 = StoreLocal(74, "dstore_3", 3)

  val AStore0 = StoreLocal(75, "astore_0", 0)
  val AStore1 = StoreLocal(76, "astore_1", 1)
  val AStore2 = StoreLocal(77, "astore_2", 2)
  val AStore3 = StoreLocal(78, "astore_3", 3)

  val IAStore = StoreArray(79, "iastore")
  val LAStore = StoreArray(80, "lastore")
  val FAStore = StoreArray(81, "fastore")
  val DAStore = StoreArray(82, "dastore")
  val AAStore = StoreArray(83, "aastore")
  val BAStore = StoreArray(84, "bastore")
  val CAStore = StoreArray(85, "castore")
  val SAStore = StoreArray(86, "sastore")

  val Pop = PureStackOpCode(87, "pop"){ case _ :: s => s }
  val Pop2 = PureStackOpCode(88, "pop2"){ case _ :: _ :: s => s }
  val Dup = PureStackOpCode(89, "dup"){ case top :: s => top :: top :: s }
  val DupX1 = PureStackOpCode(90, "dup_x1"){ case top :: x :: s => top :: x :: top :: s }
  val DupX2 = PureStackOpCode(91, "dup_x2"){ case top :: y :: x :: s => top :: y :: x :: top :: s }
  val Dup2 = PureStackOpCode(92, "dup2"){ case y :: x :: s => y :: x :: y :: x :: s }
  val Dup2X1 = PureStackOpCode(93, "dup2_x1"){ case a :: b :: x :: s => a :: b :: x :: a :: b :: s }
  val Dup2X2 = PureStackOpCode(94, "dup2_x2"){ case a :: b :: x :: y :: s => a :: b :: x :: y :: a :: b :: s }
  val Swap = PureStackOpCode(95, "swap"){ case x :: y :: s=> y :: x :: s }

  val IAdd = PureStackOpCode(96, "iadd"){ stack =>
    val (x: I) :: (y: I) :: s = stack
    (x + y) :: s
  }
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

  val IInc = OpCode(132, "iinc"){ case ctx => val index = ctx.nextByte(); ctx.frame.locals(index) = ctx.frame.locals(index).asInstanceOf[Int] + ctx.nextByte()}

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

  val IfEq = UnaryBranch(153, "ifeq")(_ == 0)
  val IfNe = UnaryBranch(154, "ifne")(_ != 0)
  val IfLt = UnaryBranch(155, "iflt")(_ < 0)
  val IfGe = UnaryBranch(156, "ifge")(_ >= 0)
  val IfGt = UnaryBranch(157, "ifgt")(_ > 0)
  val IfLe = UnaryBranch(158, "ifle")(_ <= 0)

  val IfICmpEq = BinaryBranch(159, "if_icmpeq")(_ == _)
  val IfICmpNe = BinaryBranch(160, "if_icmpne")(_ != _)
  val IfICmpLt = BinaryBranch(161, "if_icmplt")(_ < _)
  val IfICmpGe = BinaryBranch(162, "if_icmpge")(_ >= _)
  val IfICmpGt = BinaryBranch(163, "if_icmpgt")(_ > _)
  val IfICmpLe = BinaryBranch(164, "if_icmple")(_ <= _)
  val IfACmpEq = BinaryBranch(165, "if_acmpeq")(_ == _)
  val IfACmpNe = BinaryBranch(166, "if_acmpne")(_ != _)

  val Goto = UnaryBranch(167, "goto"){x => true}
  val Jsr = JsrBranch(168, "jsr")
  val Ret = RetBranch(169, "ret")
  val TableSwitch = OpCode(170, "tableswitch")()
  val LookupSwitch = OpCode(171, "lookupswitch")()

  val IReturn = OpCode(172, "ireturn"){ ctx => ctx.returnVal(Some(ctx.stack.head)) }
  val LReturn = OpCode(173, "lreturn"){ ctx => ctx.returnVal(Some(ctx.stack.head)) }
  val FReturn = OpCode(174, "freturn"){ ctx => ctx.returnVal(Some(ctx.stack.head)) }
  val DReturn = OpCode(175, "dreturn"){ ctx => ctx.returnVal(Some(ctx.stack.head)) }
  val AReturn = OpCode(176, "areturn"){ ctx => ctx.returnVal(Some(ctx.stack.head)) }
  val Return = OpCode(177, "return"){ ctx => ctx.returnVal(None) }

  val GetStatic = StackOpCode(178, "getstatic"){case (ctx, stack) =>
    val index = ctx.twoBytes()

    val FieldRef(
      ClassRef(name),
      NameAndTypeInfo(fieldName, descriptor)
    ) = ctx.rcp(index)

    val myClass = ctx.classes(name)
    myClass.statics(fieldName) :: stack
  }
  val PutStatic = StackOpCode(179, "putstatic"){case (ctx, value :: stack) =>

    val FieldRef(
      ClassRef(name),
      NameAndTypeInfo(fieldName, descriptor)
    ) = ctx.rcp(ctx.twoBytes())
    val myClass = ctx.classes(name)
    myClass.statics(fieldName) = value

    stack
  }

  val GetField = StackOpCode(180, "getfield"){case (ctx, (objectRef: svm.Object) :: stack) =>
    import ctx.{stack => _, _}; import ConstantInfo._

    val FieldRef(
      _,
      NameAndTypeInfo(fieldName, descriptor)
    ) = ctx.rcp(ctx.twoBytes())

    objectRef.members(fieldName) :: stack
  }
  val PutField = StackOpCode(181, "putfield"){case (ctx, value :: (objectRef: svm.Object) :: stack) =>
    import ctx.{stack => _, _}; import ConstantInfo._

    val FieldRef(
      _,
      NameAndTypeInfo(fieldName, descriptor)
    ) = ctx.rcp(ctx.twoBytes())


    objectRef.members(fieldName) = value
    stack
  }

  val InvokeVirtual = OpCode(182, "invokevirtual")()
  val InvokeSpecial = OpCode(183, "invokespecial")()
  val InvokeStatic = OpCode(184, "invokestatic"){case ctx =>
    import ctx._

    val MethodRef(
      ClassRef(className), NameAndTypeInfo(name, methodType)
    ) = ctx.rcp(ctx.twoBytes())
    val cls = classes(className)
    val method = cls.classFile
                    .methods
                    .find(_.name == name)
                    .get

    thread.threadStack.push(new Frame(
      runningClass = cls,
      method = method,
      locals = mutable.Seq.fill(method.code.maxLocals)(null)

    ))
    //new Array[Object](count) :: stack
  }
  val InvokeInterface = OpCode(185, "invokeinterface")()
  val InvokeDynamic = OpCode(186, "invokedynamic")()

  val New = OpCode(187, "new"){case ctx =>
    import ctx.{stack => _, _};

    val ConstantInfo.ClassRef(name) = ctx.rcp(ctx.twoBytes())

    //objectRef.members(fieldName) = value
    //stack
  }
  val NewArray = StackOpCode(188, "newarray"){case (ctx, (count: Int) :: stack) =>
    val newArray = ctx.nextByte() match{
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
  val ANewArray = StackOpCode(189, "anewarray"){case (ctx, (count: Int) :: stack) =>
    ctx.twoBytes()
    new Array[Object](count) :: stack
  }

  val ArrayLength = PureStackOpCode(190, "arraylength"){case (array: Array[_]) :: stack =>
    array.length :: stack
  }
  val AThrow = OpCode(191, "athrow")()
  val CheckCast = OpCode(192, "checkcast")()
  val InstanceOf = OpCode(193, "instanceof")()
  val MonitorEnter = OpCode(194, "monitorenter")()
  val MonitorExit = OpCode(195, "monitorexit")()
  val Wide = OpCode(196, "wide")()
  val MultiANewArray = StackOpCode(197, "multianewarray"){case (ctx, stack) =>
    val i = ctx.nextByte() << 8 | ctx.nextByte()
    val (dims, newStack) = stack.splitAt(ctx.nextByte())
    val dimArray = dims.map(x => x.asInstanceOf[Int])
    val array = java.lang.reflect.Array.newInstance(classOf[Object], dimArray:_*)
    array :: newStack
  }

  val IfNull = StackOpCode(198, "ifnull"){case (ctx, ref :: stack) =>
    if (ref == null) ctx.frame.pc = ctx.frame.pc + ctx.twoBytes()
    stack
  }
  val IfNonNull = StackOpCode(199, "ifnonnull"){case (ctx, ref :: stack) =>
    if (ref != null) ctx.frame.pc = ctx.frame.pc + ctx.twoBytes()
    stack
  }

  val GotoW = OpCode(200, "goto_w")()
  val JsrW = OpCode(201, "jsr_w")()

  def apply(n: Int) = all((n + 256) % 256)
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



