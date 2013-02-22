package svm.model

import svm.{VmThread, Frame}
import collection.mutable
import org.objectweb.asm.tree._


case class Context(thread: VmThread){
  def classes = thread.classes
  def frame = thread.threadStack.head
  def stack = frame.stack

  def jumpTo(l: LabelNode) = ???
  def throwException(exception: Any) = ???
  def returnVal(x: Option[Any]) = {
    thread.threadStack.pop()
    x.foreach(value => thread.threadStack.head.stack = value :: thread.threadStack.head.stack)
  }
}

abstract class OpCode{
  def insnName: String
  def id: Byte
  def op: Context => Unit
}

object OpCode {
  object TryParse{
    def unapply(x: AbstractInsnNode)(implicit code: () => Code) = read.lift(x)
  }
  def unapply(o: OpCode) = (o.id, o.insnName, o.op)
  implicit def getId(x: AbstractInsnNode) = x.getOpcode
  implicit def makeLazyLabel(x: LabelNode)(implicit code: () => Code) = new LazyLabel(x)
  def read(implicit code: () => Code): PartialFunction[Any, OpCode] = {
    case x: FieldInsnNode           => all(x).asInstanceOf[(String, String, String) => OpCode].apply(x.owner, x.name, x.desc)
    case x: IincInsnNode            => all(x).asInstanceOf[(Int, Int) => OpCode].apply(x.`var`, x.incr)
    case x: InsnNode                => all(x).asInstanceOf[OpCode]
    case x: IntInsnNode             => all(x).asInstanceOf[Int => OpCode].apply(x.operand)
    case x: InvokeDynamicInsnNode   => all(x).asInstanceOf[(String, String, Object, Object) => OpCode].apply(x.name, x.desc, x.bsm, x.bsmArgs)
    case x: JumpInsnNode            => all(x).asInstanceOf[LazyLabel => OpCode].apply(new LazyLabel(x.label))
    case x: LdcInsnNode             => all(x).asInstanceOf[Object => OpCode].apply(x.cst)
    case x: LookupSwitchInsnNode    => all(x).asInstanceOf[(LazyLabel, Seq[Int], Seq[LazyLabel]) => OpCode].apply(x.dflt, x.keys.safeList.map(x => x: Int), x.labels.safeList.map(x => x: LazyLabel))
    case x: MethodInsnNode          => all(x).asInstanceOf[(String, String, String) => OpCode].apply(x.owner, x.name, x.desc)
    case x: MultiANewArrayInsnNode  => all(x).asInstanceOf[(String, Int) => OpCode].apply(x.desc, x.dims)
    case x: TableSwitchInsnNode     => all(x).asInstanceOf[(Int, Int, LazyLabel, Seq[LazyLabel]) => OpCode].apply(x.min, x.max, x.dflt, x.labels.safeList.map(x => x: LazyLabel))
    case x: TypeInsnNode            => all(x).asInstanceOf[String=> OpCode].apply(x.desc)
    case x: VarInsnNode             => all(x).asInstanceOf[Int => OpCode].apply(x.`var`)
  }

  import TypeDesc._
  abstract class BaseOpCode(val id: Byte, val insnName: String) extends OpCode{
    def op: Context => Unit
  }
  implicit def intToByte(n: Int) = n.toByte

  object Nop extends OpCode{
    def insnName = "nop"
    def id = 0
    def op = _ => ()
  }

  class PushOpCode(val id: Byte, val insnName: String, value: Any) extends OpCode{
    def op = _.frame.stack ::= value
  }

  case object AConstNull extends PushOpCode(1, "aconst_null", null)
  case object IConstNull extends PushOpCode(2, "iconst_m1", -1)

  case object IConst0 extends PushOpCode(3, "iconst_0", 0)
  case object IConst1 extends PushOpCode(4, "iconst_1", 1)
  case object IConst2 extends PushOpCode(5, "iconst_2", 2)
  case object IConst3 extends PushOpCode(6, "iconst_3", 3)
  case object IConst4 extends PushOpCode(7, "iconst_4", 4)
  case object IConst5 extends PushOpCode(8, "iconst_5", 5)

  case object LConst0 extends PushOpCode(9, "lconst_0", 0L)
  case object LConst1 extends PushOpCode(10, "lconst_1", 1L)

  case object FConst0 extends PushOpCode(11, "fconst_0", 0f)
  case object FConst1 extends PushOpCode(12, "fconst_1", 1f)
  case object FConst2 extends PushOpCode(13, "fconst_2", 2f)

  case object DConst0 extends PushOpCode(14, "dconst_0", 0d)
  case object DConst1 extends PushOpCode(15, "dconst_1", 1d)

  class PushValOpCode(val id: Byte, val insnName: String, value: Int) extends OpCode{
    def op = _.frame.stack ::= value
  }

  case class BiPush(value: Int) extends PushValOpCode(16, "bipush", value)
  case class SiPush(value: Int) extends PushValOpCode(17,"sipush", value)

  class PushConstOpCode(val id: Byte, val insnName: String, const: Any) extends OpCode{
    def op = _.frame.stack ::= const
  }

  case class Ldc(const: Any) extends PushConstOpCode(18, "ldc", const)
  case class LdcW(const: Any) extends PushConstOpCode(19, "ldc_w", const)
  case class Ldc2W(const: Any) extends PushConstOpCode(20, "ldc2_w", const)

  abstract class PushLocalIndexed(val id: Byte, val insnName: String) extends OpCode{
    def op = (ctx => ctx.frame.stack ::= ctx.frame.locals(index))
    def index: Int
  }

  case class ILoad(index: Int) extends PushLocalIndexed(21, "iLoad")
  case class LLoad(index: Int) extends PushLocalIndexed(22, "lLoad")
  case class FLoad(index: Int) extends PushLocalIndexed(23, "fLoad")
  case class DLoad(index: Int) extends PushLocalIndexed(24, "dLoad")
  case class ALoad(index: Int) extends PushLocalIndexed(25, "aLoad")

  case class ILoad0(index: Int) extends PushLocalIndexed(26, "iLoad_0")
  case class ILoad1(index: Int) extends PushLocalIndexed(27, "iLoad_1")
  case class ILoad2(index: Int) extends PushLocalIndexed(28, "iLoad_2")
  case class ILoad3(index: Int) extends PushLocalIndexed(29, "iLoad_3")

  case class LLoad0(index: Int) extends PushLocalIndexed(30, "lLoad_0")
  case class LLoad1(index: Int) extends PushLocalIndexed(31, "lLoad_1")
  case class LLoad2(index: Int) extends PushLocalIndexed(32, "lLoad_2")
  case class LLoad3(index: Int) extends PushLocalIndexed(33, "lLoad_3")

  case class FLoad0(index: Int) extends PushLocalIndexed(34, "fLoad_0")
  case class FLoad1(index: Int) extends PushLocalIndexed(35, "fLoad_1")
  case class FLoad2(index: Int) extends PushLocalIndexed(36, "fLoad_2")
  case class FLoad3(index: Int) extends PushLocalIndexed(37, "fLoad_3")

  case class DLoad0(index: Int) extends PushLocalIndexed(38, "dLoad_0")
  case class DLoad1(index: Int) extends PushLocalIndexed(39, "dLoad_1")
  case class DLoad2(index: Int) extends PushLocalIndexed(40, "dLoad_2")
  case class DLoad3(index: Int) extends PushLocalIndexed(41, "dLoad_3")

  case class ALoad0(index: Int) extends PushLocalIndexed(42, "aLoad_0")
  case class ALoad1(index: Int) extends PushLocalIndexed(43, "aLoad_1")
  case class ALoad2(index: Int) extends PushLocalIndexed(44, "aLoad_2")
  case class ALoad3(index: Int) extends PushLocalIndexed(45, "aLoad_3")

  class PushFromArray(val id: Byte, val insnName: String) extends OpCode{
    def op = ctx => {
      val (index: Int) :: (array: Array[Any]) :: stack = ctx.frame.stack
      ctx.frame.stack = array(index) :: stack
    }
  }

  object IALoad extends PushFromArray(46, "iaLoad")
  object LALoad extends PushFromArray(47, "laLoad")
  object FALoad extends PushFromArray(48, "faLoad")
  object DALoad extends PushFromArray(49, "daLoad")
  object AALoad extends PushFromArray(50, "aaLoad")
  object BALoad extends PushFromArray(51, "baLoad")
  object CALoad extends PushFromArray(52, "caLoad")
  object SALoad extends PushFromArray(53, "saLoad")

  abstract class StoreLocal(val id: Byte, val insnName: String) extends OpCode{
    def varId: Int
    def op = ctx => {
      val top :: stack = ctx.stack
      ctx.frame.locals(varId) = top
      ctx.frame.stack = stack
    }
  }
  case class IStore(varId: Int) extends StoreLocal(54, "istore")
  case class LStore(varId: Int) extends StoreLocal(55, "lstore")
  case class FStore(varId: Int) extends StoreLocal(56, "fstore")
  case class DStore(varId: Int) extends StoreLocal(57, "dstore")
  case class AStore(varId: Int) extends StoreLocal(58, "astore")

  case class IStore0(varId: Int) extends StoreLocal(59, "istore_0")
  case class IStore1(varId: Int) extends StoreLocal(60, "istore_1")
  case class IStore2(varId: Int) extends StoreLocal(61, "istore_2")
  case class IStore3(varId: Int) extends StoreLocal(62, "istore_3")

  case class LStore0(varId: Int) extends StoreLocal(63, "lstore_0")
  case class LStore1(varId: Int) extends StoreLocal(64, "lstore_1")
  case class LStore2(varId: Int) extends StoreLocal(65, "lstore_2")
  case class LStore3(varId: Int) extends StoreLocal(66, "lstore_3")

  case class FStore0(varId: Int) extends StoreLocal(67, "fstore_0")
  case class FStore1(varId: Int) extends StoreLocal(68, "fstore_1")
  case class FStore2(varId: Int) extends StoreLocal(69, "fstore_2")
  case class FStore3(varId: Int) extends StoreLocal(70, "fstore_3")

  case class DStore0(varId: Int) extends StoreLocal(71, "dstore_0")
  case class DStore1(varId: Int) extends StoreLocal(72, "dstore_1")
  case class DStore2(varId: Int) extends StoreLocal(73, "dstore_2")
  case class DStore3(varId: Int) extends StoreLocal(74, "dstore_3")

  case class AStore0(varId: Int) extends StoreLocal(75, "astore_0")
  case class AStore1(varId: Int) extends StoreLocal(76, "astore_1")
  case class AStore2(varId: Int) extends StoreLocal(77, "astore_2")
  case class AStore3(varId: Int) extends StoreLocal(78, "astore_3")

  class StoreArray(val id: Byte, val insnName: String) extends OpCode{
    def op = ctx => {
      val value :: (index: Int) :: (array: Array[Any]) :: stack = ctx.frame.stack
      array(index) = value
      ctx.frame.stack = stack
    }
  }
  object IAStore extends StoreArray(79, "iastore")
  object LAStore extends StoreArray(80, "lastore")
  object FAStore extends StoreArray(81, "fastore")
  object DAStore extends StoreArray(82, "dastore")
  object AAStore extends StoreArray(83, "aastore")
  object BAStore extends StoreArray(84, "bastore")
  object CAStore extends StoreArray(85, "castore")
  object SAStore extends StoreArray(86, "sastore")

  class PureStackOpCode(val id: Byte, val insnName: String)(transform: List[Any] => List[Any]) extends OpCode{
     def op = ctx => ctx.frame.stack = transform(ctx.stack)
  }
  object Pop extends PureStackOpCode(87, "pop")({ case _ :: s => s })
  object Pop2 extends PureStackOpCode(88, "pop2")({ case _ :: _ :: s => s })
  object Dup extends PureStackOpCode(89, "dup")({ case top :: s => top :: top :: s })
  object DupX1 extends PureStackOpCode(90, "dup_x1")({ case top :: x :: s => top :: x :: top :: s })
  object DupX2 extends PureStackOpCode(91, "dup_x2")({ case top :: y :: x :: s => top :: y :: x :: top :: s })
  object Dup2 extends PureStackOpCode(92, "dup2")({ case y :: x :: s => y :: x :: y :: x :: s })
  object Dup2X1 extends PureStackOpCode(93, "dup2_x1")({ case a :: b :: x :: s => a :: b :: x :: a :: b :: s })
  object Dup2X2 extends PureStackOpCode(94, "dup2_x2")({ case a :: b :: x :: y :: s => a :: b :: x :: y :: a :: b :: s })
  object Swap extends PureStackOpCode(95, "swap")({ case x :: y :: s=> y :: x :: s })

  object IAdd extends PureStackOpCode(96, "iadd")({ case (x: I) :: (y: I) :: s => (x + y) :: s })
  object LAdd extends PureStackOpCode(97, "ladd")({ case (x: J) :: (y: J) :: s => (y + x) :: s})
  object FAdd extends PureStackOpCode(98, "fadd")({ case (x: F) :: (y: F) :: s => (y + x) :: s})
  object DAdd extends PureStackOpCode(99, "dadd")({ case (x: D) :: (y: D) :: s => (y + x) :: s})

  object ISub extends PureStackOpCode(100, "isub")({ case (x: I) :: (y: I) :: s => (y - x) :: s})
  object LSub extends PureStackOpCode(101, "lsub")({ case (x: J) :: (y: J) :: s => (y - x) :: s})
  object FSub extends PureStackOpCode(102, "fsub")({ case (x: F) :: (y: F) :: s => (y - x) :: s})
  object DSub extends PureStackOpCode(103, "dsub")({ case (x: D) :: (y: D) :: s => (y - x) :: s})

  object IMul extends PureStackOpCode(104, "imul")({ case (x: I) :: (y: I) :: s => (y * x) :: s})
  object LMul extends PureStackOpCode(105, "lmul")({ case (x: J) :: (y: J) :: s => (y * x) :: s})
  object FMul extends PureStackOpCode(106, "fmul")({ case (x: F) :: (y: F) :: s => (y * x) :: s})
  object DMul extends PureStackOpCode(107, "dmul")({ case (x: D) :: (y: D) :: s => (y * x) :: s})

  object IDiv extends PureStackOpCode(108, "idiv")({ case (x: I) :: (y: I) :: s => (y / x) :: s})
  object LDiv extends PureStackOpCode(109, "ldiv")({ case (x: J) :: (y: J) :: s => (y / x) :: s})
  object FDiv extends PureStackOpCode(110, "fdiv")({ case (x: F) :: (y: F) :: s => (y / x) :: s})
  object DDiv extends PureStackOpCode(111, "ddiv")({ case (x: D) :: (y: D) :: s => (y / x) :: s})

  object IRem extends PureStackOpCode(112, "irem")({ case (x: I) :: (y: I) :: s => (y % x) :: s})
  object LRem extends PureStackOpCode(113, "lrem")({ case (x: J) :: (y: J) :: s => (y % x) :: s})
  object FRem extends PureStackOpCode(114, "frem")({ case (x: F) :: (y: F) :: s => (y % x) :: s})
  object DRem extends PureStackOpCode(115, "drem")({ case (x: D) :: (y: D) :: s => (y % x) :: s})

  object INeg extends PureStackOpCode(116, "ineg")({ case (x: I) :: s => -x :: s })
  object LNeg extends PureStackOpCode(117, "lneg")({ case (x: J) :: s => -x :: s })
  object FNeg extends PureStackOpCode(118, "fneg")({ case (x: F) :: s => -x :: s })
  object DNeg extends PureStackOpCode(119, "dneg")({ case (x: D) :: s => -x :: s })

  object IShl extends PureStackOpCode(120, "ishl")({ case (x: I) :: (y: I) :: s => (x << y) :: s })
  object LShl extends PureStackOpCode(121, "lshl")({ case (x: J) :: (y: J) :: s => (x << y) :: s })
  object IShr extends PureStackOpCode(122, "ishr")({ case (x: I) :: (y: I) :: s => (x >> y) :: s })
  object LShr extends PureStackOpCode(123, "lshr")({ case (x: J) :: (y: J) :: s => (x >> y) :: s })

  object IUShr extends PureStackOpCode(124, "iushr")({ case (x: I) :: (y: I) :: s => (x >>> y) :: s })
  object LUShr extends PureStackOpCode(125, "lushr")({ case (x: J) :: (y: J) :: s => (x >>> y) :: s })

  object IAnd extends PureStackOpCode(126, "iand")({ case s :+ (x: I) :+ (y: I) => s :+ (x & y) })
  object LAnd extends PureStackOpCode(127, "land")({ case s :+ (x: J) :+ (y: I) => s :+ (x & y) })

  object IOr extends PureStackOpCode(128, "ior")({ case s :+ (x: I) :+ (y: I) => s :+ (x | y) })
  object LOr extends PureStackOpCode(129, "lor")({ case s :+ (x: J) :+ (y: I) => s :+ (x | y) })

  object IXOr extends PureStackOpCode(130, "ixor")({ case s :+ (x: I) :+ (y: I) => s :+ (x ^ y) })
  object LXOr extends PureStackOpCode(131, "lxor")({ case s :+ (x: J) :+ (y: I) => s :+ (x ^ y) })

  case class IInc(varId: Int, amount: Int) extends OpCode{
    def id = 132
    def insnName = "iinc"
    def op = ctx => ctx.frame.locals(varId) = (ctx.frame.locals(varId).asInstanceOf[Int]) + amount
  }

  object I2L extends PureStackOpCode(133, "i2l")({ case (x: I) :: s => x.toLong :: s})
  object I2F extends PureStackOpCode(134, "i2f")({ case (x: I) :: s => x.toFloat :: s })
  object I2D extends PureStackOpCode(135, "i2d")({ case (x: I) :: s => x.toDouble :: s })

  object L2I extends PureStackOpCode(136, "l2i")({ case (x: J) :: s => x.toInt :: s })
  object L2F extends PureStackOpCode(137, "l2f")({ case (x: J) :: s => x.toFloat :: s })
  object L2D extends PureStackOpCode(138, "l2d")({ case (x: J) :: s => x.toDouble :: s })

  object F2I extends PureStackOpCode(139, "f2i")({ case (x: F) :: s => x.toInt :: s })
  object F2L extends PureStackOpCode(140, "f2l")({ case (x: F) :: s => x.toLong :: s })
  object F2D extends PureStackOpCode(141, "f2d")({ case (x: F) :: s => x.toDouble :: s })

  object D2I extends PureStackOpCode(142, "d2i")({ case (x: D) :: s => x.toInt :: s })
  object D2L extends PureStackOpCode(143, "d2l")({ case (x: D) :: s => x.toLong :: s })
  object D2F extends PureStackOpCode(144, "d2f")({ case (x: D) :: s  => x.toFloat :: s })

  object I2B extends PureStackOpCode(145, "i2b")({ case (x: I) :: s => x.toByte :: s })
  object I2C extends PureStackOpCode(146, "i2c")({ case (x: I) :: s => x.toChar :: s })
  object I2S extends PureStackOpCode(147, "i2s")({ case (x: I) :: s => x.toShort :: s })

  object LCmp extends PureStackOpCode(148, "lcmp")({ case (x: J) :: (y: J) :: s => x.compare(y) :: s })
  object FCmpl extends PureStackOpCode(149, "fcmpl")({ case (x: F) :: (y: F) :: s => x.compare(y) :: s })
  object FCmpg extends PureStackOpCode(150, "fcmpg")({ case (x: F) :: (y: F) :: s => x.compare(y) :: s })
  object DCmpl extends PureStackOpCode(151, "dcmpl")({ case (x: D) :: (y: D) :: s => x.compare(y) :: s })
  object DCmpg extends PureStackOpCode(152, "dcmpg")({ case (x: D) :: (y: D) :: s => x.compare(y) :: s })


  abstract class UnaryBranch(val id: Byte, val insnName: String)(pred: Int => Boolean) extends OpCode{

    def label: LabelNode
    def op = ctx => {
      val (top: Int) :: stack = ctx.stack
      if(pred(top)) ctx.jumpTo(label)
    }
  }

  case class IfEq(label: LabelNode) extends UnaryBranch(153, "ifeq")(_ == 0)
  case class IfNe(label: LabelNode) extends UnaryBranch(154, "ifne")(_ != 0)
  case class IfLt(label: LabelNode) extends UnaryBranch(155, "iflt")(_ < 0)
  case class IfGe(label: LabelNode) extends UnaryBranch(156, "ifge")(_ >= 0)
  case class IfGt(label: LabelNode) extends UnaryBranch(157, "ifgt")(_ > 0)
  case class IfLe(label: LabelNode) extends UnaryBranch(158, "ifle")(_ <= 0)

  abstract class BinaryBranch(val id: Byte, val insnName: String)(pred: (Int, Int) => Boolean) extends OpCode{
    def label: LabelNode
    def op = ctx => {
      val (top: Int) :: (next: Int) :: stack = ctx.stack
      if(pred(top, next)) ctx.jumpTo(label)
    }
  }

  case class IfICmpEq(label: LabelNode) extends BinaryBranch(159, "if_icmpeq")(_ == _)
  case class IfICmpNe(label: LabelNode) extends BinaryBranch(160, "if_icmpne")(_ != _)
  case class IfICmpLt(label: LabelNode) extends BinaryBranch(161, "if_icmplt")(_ < _)
  case class IfICmpGe(label: LabelNode) extends BinaryBranch(162, "if_icmpge")(_ >= _)
  case class IfICmpGt(label: LabelNode) extends BinaryBranch(163, "if_icmpgt")(_ > _)
  case class IfICmpLe(label: LabelNode) extends BinaryBranch(164, "if_icmple")(_ <= _)
  case class IfACmpEq(label: LabelNode) extends BinaryBranch(165, "if_acmpeq")(_ == _)
  case class IfACmpNe(label: LabelNode) extends BinaryBranch(166, "if_acmpne")(_ != _)

  case class Goto(label: LabelNode) extends UnaryBranch(157, "goto")(x => true)

  case object Jsr extends BaseOpCode(168, "jsr"){ def op = ??? }
  case object Ret extends BaseOpCode(169, "ret"){ def op = ??? }
  case object TableSwitch extends BaseOpCode(170, "tableswitch"){ def op = ??? }
  case object LookupSwitch extends BaseOpCode(171, "lookupswitch"){ def op = ??? }

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
    def op = ctx => {
      val value :: stack = ctx.stack
      ctx.classes(owner).statics(name) = value
      ctx.frame.stack = stack
    }
  }

  case class GetField(owner: String, name: String, desc: String) extends BaseOpCode(180, "getfield"){
    def op = ctx => {
      val (objectRef: svm.Object) :: stack = ctx.stack
      ctx.frame.stack = objectRef.members(name) :: stack
    }
  }
  case class PutField(owner: String, name: String, desc: String) extends BaseOpCode(181, "putfield"){
    def op = ctx => {
      val value :: (objectRef: svm.Object) :: stack = ctx.stack
      objectRef.members(name) = value
      ctx.frame.stack = stack
    }
  }

  case class InvokeVirtual(owner: String, name: String, desc: String) extends BaseOpCode(182, "invokevirtual"){def op = ???}
  case class InvokeSpecial(owner: String, name: String, desc: String) extends BaseOpCode(183, "invokespecial"){def op = ???}
  case class InvokeStatic(owner: String, name: String, desc: String) extends BaseOpCode(184, "invokestatic"){

    def op = ctx => {
      import ctx._

      val cls = classes(owner)
      val method = cls.classFile
                      .methods
                      .find(_.name == name)
                      .get

      thread.threadStack.push(new Frame(
        runningClass = cls,
        method = method,
        locals = mutable.Seq.fill(method.misc.maxLocals)(null)

      ))
      //new Array[Object](count) :: stack
    }
  }
  case class InvokeInterface(owner: String, name: String, desc: String) extends BaseOpCode(185, "invokeinterface"){ def op = ??? }

  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends BaseOpCode(186, "invokedynamic"){ def op = ??? }

  case class New(desc: String) extends BaseOpCode(187, "new"){ def op = ??? }
  case class NewArray(typeCode: Int) extends BaseOpCode(188, "newarray"){
    def op = ctx => {
      val (count: Int) :: stack = ctx.stack
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
    def op = ctx => {
      val (count: Int) :: stack = ctx.stack
      ctx.frame.stack = new Array[Object](count) :: stack
    }
  }

  case object ArrayLength extends PureStackOpCode(190, "arraylength")({case (array: Array[_]) :: stack => array.length :: stack })
    
  case object AThrow extends BaseOpCode(191, "athrow"){
    def op = ctx => {
      val exception :: stack = ctx.stack
      ctx.throwException(exception)
    }
  }
  case class CheckCast(desc: String) extends BaseOpCode(192, "checkcast"){def op = ??? }
  case class InstanceOf(desc: String) extends BaseOpCode(193, "instanceof"){ def op = ??? }
  case object MonitorEnter extends BaseOpCode(194, "monitorenter"){ def op = ???  }
  case object MonitorExit extends BaseOpCode(195, "monitorexit"){ def op = ??? }
  case object Wide extends BaseOpCode(196, "wide"){ def op = ??? }
  case class MultiANewArray(desc: String, dims: Int) extends BaseOpCode(197, "multianewarray"){

    def op = ctx => {
      val (dimValues, newStack) = ctx.stack.splitAt(dims)
      val dimArray = dimValues.map(x => x.asInstanceOf[Int])
      val array = java.lang.reflect.Array.newInstance(classOf[Object], dimArray:_*)
      ctx.frame.stack = array :: newStack
    }
  }

  case class IfNull(label: LabelNode) extends BaseOpCode(198, "ifnull"){
    def op = ctx => {
      val ref :: stack = ctx.stack
      if (ref == null) ctx.jumpTo(label)
      ctx.frame.stack = stack
    }
  }

  case class IfNonNull(label: LabelNode) extends BaseOpCode(199, "ifnonnull"){
    def op = ctx => {
      val ref :: stack = ctx.stack
      if (ref != null) ctx.jumpTo(label)
      ctx.frame.stack = stack
    }
  }

  case object GotoW extends BaseOpCode(200, "goto_w"){ def op = ??? }
  case object JsrW extends BaseOpCode(201, "jsr_w"){ def op = ??? }

  def apply(n: Int): Any = all((n + 256) % 256)
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




