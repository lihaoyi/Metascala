package svm


object OpCodes {

  import OpCodeGen._

  val codeMap = allCodes.map(x => x.id -> x).toMap
  val allCodes: Set[OpCode] = Set[OpCode](
    OpCode(0, "nop"){ (inst, fstack) => fstack},
    PushOp(1, "aconst_null", null),
    PushOp(2, "iconst_m1", -1),

    PushOp(3, "iconst_0", 0),
    PushOp(4, "iconst_1", 1),
    PushOp(5, "iconst_2", 2),
    PushOp(6, "iconst_3", 3),
    PushOp(7, "iconst_4", 4),
    PushOp(8, "iconst_5", 5),

    PushOp(9, "lconst_0", 0L),
    PushOp(10, "lconst_1", 1L),

    PushOp(11, "fconst_0", 0f),
    PushOp(12, "fconst_1", 1f),
    PushOp(13, "fconst_2", 2f),

    PushOp(14, "dconst_0", 0d),
    PushOp(15, "dconst_1", 1d),

    PushValOp(16, "bipush", b => b(1)),
    PushValOp(17,"sipush", b => b(1) << 8 + b(2)),

    pushConst(18, "ldc", b => b(1)),
    pushConst(19, "ldc_w", b => b(1) << 8 + b(2)),
    pushConst(20, "ldc2_w", b => b(1) << 8 + b(2)),

    PushLocalIndexed(21, "iload", -1),
    PushLocalIndexed(22, "lload", -1),
    PushLocalIndexed(23, "fload", -1),
    PushLocalIndexed(24, "dload", -1),
    PushLocalIndexed(25, "aload", -1),

    PushLocalIndexed(26, "iload_0", 0),
    PushLocalIndexed(27, "iload_1", 1),
    PushLocalIndexed(28, "iload_2", 2),
    PushLocalIndexed(29, "iload_3", 3),

    PushLocalIndexed(30, "lload_0", 0),
    PushLocalIndexed(31, "lload_1", 1),
    PushLocalIndexed(32, "lload_2", 2),
    PushLocalIndexed(33, "lload_3", 3),

    PushLocalIndexed(34, "fload_0", 0),
    PushLocalIndexed(35, "fload_1", 1),
    PushLocalIndexed(36, "fload_2", 2),
    PushLocalIndexed(37, "fload_3", 3),

    PushLocalIndexed(38, "dload_0", 0),
    PushLocalIndexed(39, "dload_1", 1),
    PushLocalIndexed(40, "dload_2", 2),
    PushLocalIndexed(41, "dload_3", 3),

    PushLocalIndexed(42, "aload_0", 0),
    PushLocalIndexed(43, "aload_1", 1),
    PushLocalIndexed(44, "aload_2", 2),
    PushLocalIndexed(45, "aload_3", 3),

    PushFromArray(46, "iaload"),
    PushFromArray(47, "laload"),
    PushFromArray(48, "faload"),
    PushFromArray(49, "daload"),
    PushFromArray(50, "aaload"),
    PushFromArray(51, "baload"),
    PushFromArray(52, "caload"),
    PushFromArray(53, "saload"),

    StoreLocal(54, "istore", -1),
    StoreLocal(55, "lstore", -1),
    StoreLocal(56, "fstore", -1),
    StoreLocal(57, "dstore", -1),
    StoreLocal(58, "astore", -1),

    StoreLocal(59, "istore_0", 0),
    StoreLocal(60, "istore_1", 1),
    StoreLocal(61, "istore_2", 2),
    StoreLocal(62, "istore_3", 3),

    StoreLocal(63, "lstore_0", 0),
    StoreLocal(64, "lstore_1", 1),
    StoreLocal(65, "lstore_2", 2),
    StoreLocal(66, "lstore_3", 3),

    StoreLocal(67, "fstore_0", 0),
    StoreLocal(68, "fstore_1", 1),
    StoreLocal(69, "fstore_2", 2),
    StoreLocal(70, "fstore_3", 3),

    StoreLocal(71, "dstore_0", 0),
    StoreLocal(72, "dstore_1", 1),
    StoreLocal(73, "dstore_2", 2),
    StoreLocal(74, "dstore_3", 3),

    StoreLocal(75, "astore_0", 0),
    StoreLocal(76, "astore_1", 1),
    StoreLocal(77, "astore_2", 2),
    StoreLocal(78, "astore_3", 3),

    storeArray(79, "iastore"),
    storeArray(80, "lastore"),
    storeArray(81, "fastore"),
    storeArray(82, "dastore"),
    storeArray(83, "aastore"),
    storeArray(84, "bastore"),
    storeArray(85, "castore"),
    storeArray(86, "dastore"),

    StackManip(87, "pop"){ case s :+ top => s },
    StackManip(88, "pop2"){ case s :+ second :+ top => s },
    StackManip(89, "dup"){ case s :+ top => s :+ top :+ top },
    StackManip(90, "dup_x1"){ case s :+ x :+ top => s :+ top :+ x :+ top },
    StackManip(91, "dup_x2"){ case s :+ x :+ y :+ top => s :+ top :+ x :+ y :+ top },
    StackManip(92, "dup"){ case s :+ x :+ y=> s :+ x :+ y :+ x :+ y },
    StackManip(93, "dup2_x1"){ case s :+ x :+ top2 :+ top => s :+ top2 :+ top :+ x :+ top2 :+ top },
    StackManip(94, "dup2_x2"){ case s :+ x :+ y :+ top2 :+ top => s :+ top2 :+ top :+ x :+ y :+ top2 :+ top },
    StackManip(95, "swap"){ case s :+ x :+ y => s :+ y :+ x },

    StackManip(96, "iadd"){ case s :+ (x: Int) :+ (y: Int) => s :+ (y + x) },
    StackManip(97, "ladd"){ case s :+ (x: Long) :+ (y: Long) => s :+ (y + x) },
    StackManip(98, "fadd"){ case s :+ (x: Float) :+ (y: Float) => s :+ (y + x) },
    StackManip(99, "dadd"){ case s :+ (x: Double) :+ (y: Double) => s :+ (y + x) },

    StackManip(100, "isub"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x - y) },
    StackManip(101, "lsub"){ case s :+ (x: Long) :+ (y: Long) => s :+ (x - y) },
    StackManip(102, "fsub"){ case s :+ (x: Float) :+ (y: Float) => s :+ (x - y) },
    StackManip(103, "dsub"){ case s :+ (x: Double) :+ (y: Double) => s :+ (x - y) },

    StackManip(104, "imul"){ case s :+ (x: Int) :+ (y: Int) => s :+ (y * x) },
    StackManip(105, "lmul"){ case s :+ (x: Long) :+ (y: Long) => s :+ (y * x) },
    StackManip(106, "fmul"){ case s :+ (x: Float) :+ (y: Float) => s :+ (y * x) },
    StackManip(107, "dmul"){ case s :+ (x: Double) :+ (y: Double) => s :+ (y * x) },

    StackManip(108, "idiv"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x / y) },
    StackManip(109, "ldiv"){ case s :+ (x: Long) :+ (y: Long) => s :+ (x / y) },
    StackManip(110, "fdiv"){ case s :+ (x: Float) :+ (y: Float) => s :+ (x / y) },
    StackManip(111, "ddiv"){ case s :+ (x: Double) :+ (y: Double) => s :+ (x / y) },

    StackManip(112, "irem"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x % y) },
    StackManip(113, "lrem"){ case s :+ (x: Long) :+ (y: Long) => s :+ (x % y) },
    StackManip(114, "frem"){ case s :+ (x: Float) :+ (y: Float) => s :+ (x % y) },
    StackManip(115, "drem"){ case s :+ (x: Double) :+ (y: Double) => s :+ (x % y) },

    StackManip(116, "ineg"){ case s :+ (x: Int) => s :+ -x },
    StackManip(117, "lneg"){ case s :+ (x: Long) => s :+ -x },
    StackManip(118, "fneg"){ case s :+ (x: Float) => s :+ -x },
    StackManip(119, "dneg"){ case s :+ (x: Double) => s :+ -x },

    StackManip(120, "ishl"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x << y) },
    StackManip(121, "lshl"){ case s :+ (x: Long) :+ (y: Long) => s :+ (x << y) },
    StackManip(122, "ishr"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x >> y) },
    StackManip(123, "lshr"){ case s :+ (x: Long) :+ (y: Long) => s :+ (x >> y) },

    StackManip(124, "iushr"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x >>> y) },
    StackManip(125, "lushr"){ case s :+ (x: Long) :+ (y: Int) => s :+ (x >>> y) },

    StackManip(126, "iand"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x & y) },
    StackManip(127, "land"){ case s :+ (x: Long) :+ (y: Int) => s :+ (x & y) },

    StackManip(128, "ior"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x | y) },
    StackManip(129, "lor"){ case s :+ (x: Long) :+ (y: Int) => s :+ (x | y) },

    StackManip(130, "ixor"){ case s :+ (x: Int) :+ (y: Int) => s :+ (x ^ y) },
    StackManip(131, "lxor"){ case s :+ (x: Long) :+ (y: Int) => s :+ (x ^ y) },

    OpCode(132, "iinc"){ case (Seq(index, const), VMThread(pc, fstack :+ Frame(locals, stack, rcp))) => VMThread(pc, fstack :+ Frame(locals.updated(index, locals(index).asInstanceOf[Int] + const), stack, rcp))},

    StackManip(133, "i2l"){ case s :+ (x: Int) => s :+ x.toLong },
    StackManip(134, "i2f"){ case s :+ (x: Int) => s :+ x.toFloat },
    StackManip(135, "i2d"){ case s :+ (x: Int) => s :+ x.toDouble },

    StackManip(136, "l2i"){ case s :+ (x: Long) => s :+ x.toInt },
    StackManip(137, "l2f"){ case s :+ (x: Long) => s :+ x.toFloat },
    StackManip(138, "l2d"){ case s :+ (x: Long) => s :+ x.toDouble },

    StackManip(139, "f2i"){ case s :+ (x: Float) => s :+ x.toInt },
    StackManip(140, "f2l"){ case s :+ (x: Float) => s :+ x.toLong },
    StackManip(141, "f2d"){ case s :+ (x: Float) => s :+ x.toDouble },

    StackManip(142, "d2i"){ case s :+ (x: Double) => s :+ x.toInt },
    StackManip(143, "d2l"){ case s :+ (x: Double) => s :+ x.toLong },
    StackManip(144, "d2f"){ case s :+ (x: Double) => s :+ x.toFloat },

    StackManip(145, "i2b"){ case s :+ (x: Int) => s :+ x.toByte },
    StackManip(146, "i2c"){ case s :+ (x: Int) => s :+ x.toChar },
    StackManip(147, "i2s"){ case s :+ (x: Int) => s :+ x.toShort },

    StackManip(148, "lcmp"){ case s :+ (x: Long) :+ (y: Long) => s :+ x.compare(y) },
    StackManip(149, "fcmpl"){ case s :+ (x: Float) :+ (y: Float) => s :+ x.compare(y) },
    StackManip(150, "fcmpg"){ case s :+ (x: Float) :+ (y: Float) => s :+ x.compare(y) },
    StackManip(151, "dcmpl"){ case s :+ (x: Double) :+ (y: Double) => s :+ x.compare(y) },
    StackManip(152, "dcmpg"){ case s :+ (x: Double) :+ (y: Double) => s :+ x.compare(y) },

    UnaryBranch(153, "ifeq")(_ == 0),
    UnaryBranch(154, "ifne")(_ != 0),
    UnaryBranch(155, "iflt")(_ < 0),
    UnaryBranch(156, "ifge")(_ >= 0),
    UnaryBranch(157, "ifgt")(_ > 0),
    UnaryBranch(158, "ifle")(_ <= 0),

    BinaryBranch(159, "if_icmpeq")(_ == _),
    BinaryBranch(160, "if_icmpne")(_ != _),
    BinaryBranch(161, "if_icmplt")(_ < _),
    BinaryBranch(162, "if_icmpge")(_ >= _),
    BinaryBranch(163, "if_icmpgt")(_ > _),
    BinaryBranch(164, "if_icmple")(_ <= _),
    BinaryBranch(165, "if_icmpeq")(_ == _),
    BinaryBranch(166, "if_icmpne")(_ != _),

    UnaryBranch(167, "goto"){x => true},
    JsrBranch(168, "jsr"),
    RetBranch(169, "ret"),
    OpCode(170, "tableswitch")(???),
    OpCode(171, "lookupswitch")(???),

    ReturnBranch(172, "ireturn"),
    ReturnStuff(173, "lreturn"),
    ReturnStuff(174, "freturn"),
    ReturnStuff(175, "dreturn"),
    ReturnStuff(176, "areturn"),
    ReturnStuff(177, "return"),

    OpCode(178, "getstatic")(???),
    OpCode(179, "putstatic")(???),

    OpCode(180, "getfield")(???),
    OpCode(181, "putfield")(???),

    OpCode(182, "invokevirtual")(???),
    OpCode(183, "invokespecial")(???),
    OpCode(184, "invokestatic")(???),
    OpCode(185, "invokeinterface")(???),
    OpCode(186, "invokedynamic")(???),

    OpCode(187, "new")(???),
    OpCode(188, "newarray")(???),
    OpCode(189, "anewarray")(???),

    OpCode(190, "arraylength")(???),
    OpCode(191, "athrow")(???),
    OpCode(192, "checkcast")(???),
    OpCode(193, "instanceof")(???),
    OpCode(194, "monitorenter")(???),
    OpCode(195, "monitorexit")(???),
    OpCode(196, "wide")(???),
    OpCode(197, "multianewarray")(???),

    OpCode(198, "ifnull")(???),
    OpCode(199, "ifnonnull")(???),

    OpCode(200, "goto_w")(???),
    OpCode(201, "jsr_w")(???)

  )
}



