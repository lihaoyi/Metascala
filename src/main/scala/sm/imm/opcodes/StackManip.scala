package sm
package imm
package opcodes
import scala.collection.mutable
import sm.imm.Type.Primitives._



import scala.::
import java.util

object StackManip {
  class PureStackOpCode(val id: Byte, val insnName: String)(transform: PartialFunction[mutable.Stack[virt.StackVal], virt.StackVal]) extends OpCode{
    def op = vt => vt.push(transform(vt.frame.stack))
  }
  object S2{
    def unapply(x: mutable.Stack[virt.StackVal]) = Some((x.pop, x.pop))
  }
  object S1{
    def unapply(x: mutable.Stack[virt.StackVal]) = Some((x.pop))
  }

  class ManipOpCode(val id: Byte, val insnName: String)(transform: List[virt.StackVal] => List[virt.StackVal]) extends OpCode{
    def op = vt => {
      val in = List.fill(vt.frame.stack.length min 4)(vt.pop)
      val out = transform(in)
      out.reverseMap(vt.push)
    }
  }
  case object Pop extends ManipOpCode(87, "pop")({ case _ :: s => s })
  case object Pop2 extends ManipOpCode(88, "pop2")({
    case Cat1(_) :: Cat1(_) :: s => s
    case Cat2(_) :: s => s
  })
  case object Dup extends ManipOpCode(89, "dup")({ case top :: s => top :: top :: s })
  case object DupX1 extends ManipOpCode(90, "dup_x1")({ case top :: x :: s => top :: x :: top :: s })
  case object DupX2 extends ManipOpCode(91, "dup_x2")({
    case Cat1(top) :: Cat1(y) :: Cat1(x) :: s => top :: y :: x :: top :: s
    case Cat1(top) :: Cat2(x) :: s => top :: x :: top :: s
  })
  case object Dup2 extends ManipOpCode(92, "dup2")({
    case Cat1(y) :: Cat1(x) :: s => y :: x :: y :: x :: s
    case Cat2(x) :: s => x :: x :: s
  })
  case object Dup2X1 extends ManipOpCode(93, "dup2_x1")({
    case Cat1(a) :: Cat1(b) :: Cat1(x) :: s => a :: b :: x :: a :: b :: s
    case Cat2(a) :: Cat1(x) :: s => a :: a :: x :: a :: s
  })
  case object Dup2X2 extends ManipOpCode(94, "dup2_x2")({
    case Cat1(a) :: Cat1(b) :: Cat1(x) :: Cat1(y) :: s => a :: b :: x :: y :: a :: b :: s
    case Cat2(a) :: Cat1(b) :: Cat1(x) :: s => a :: b :: x :: a :: s
    case Cat1(a) :: Cat1(b) :: Cat2(x) :: s => a :: b :: x :: a :: b :: s
    case Cat2(a) :: Cat2(b) :: s => a :: b :: a :: s
  })
  case object Swap extends ManipOpCode(95, "swap")({ case x :: y :: s=> y :: x :: s })


  case object IAdd extends PureStackOpCode(96, "iadd")({ case S2(x: I, y: I) => x.v + y})
  case object LAdd extends PureStackOpCode(97, "ladd")({ case S2(x: J, y: J) => y.v + x})
  case object FAdd extends PureStackOpCode(98, "fadd")({ case S2(x: F, y: F)  => y.v + x})
  case object DAdd extends PureStackOpCode(99, "dadd")({ case S2(x: D, y: D)  => y.v + x})

  case object ISub extends PureStackOpCode(100, "isub")({ case S2(x: I, y: I)  => (y - x) })
  case object LSub extends PureStackOpCode(101, "lsub")({ case S2(x: J, y: J)  => (y - x) })
  case object FSub extends PureStackOpCode(102, "fsub")({ case S2(x: F, y: F)  => (y - x) })
  case object DSub extends PureStackOpCode(103, "dsub")({ case S2(x: D, y: D)  => (y - x) })

  case object IMul extends PureStackOpCode(104, "imul")({ case S2(x: I, y: I)  => (y * x) })
  case object LMul extends PureStackOpCode(105, "lmul")({ case S2(x: J, y: J)  => (y * x) })
  case object FMul extends PureStackOpCode(106, "fmul")({ case S2(x: F, y: F)  => (y * x) })
  case object DMul extends PureStackOpCode(107, "dmul")({ case S2(x: D, y: D)  => (y * x) })

  case object IDiv extends PureStackOpCode(108, "idiv")({ case S2(x: I, y: I)  => (y / x) })
  case object LDiv extends PureStackOpCode(109, "ldiv")({ case S2(x: J, y: J)  => (y / x) })
  case object FDiv extends PureStackOpCode(110, "fdiv")({ case S2(x: F, y: F)  => (y / x) })
  case object DDiv extends PureStackOpCode(111, "ddiv")({ case S2(x: D, y: D)  => (y / x) })

  case object IRem extends PureStackOpCode(112, "irem")({ case S2(x: I, y: I)  => (y % x) })
  case object LRem extends PureStackOpCode(113, "lrem")({ case S2(x: J, y: J)  => (y % x) })
  case object FRem extends PureStackOpCode(114, "frem")({ case S2(x: F, y: F)  => (y % x) })
  case object DRem extends PureStackOpCode(115, "drem")({ case S2(x: D, y: D)  => (y % x) })

  case object INeg extends PureStackOpCode(116, "ineg")({ case S1(x: I)  => -x  })
  case object LNeg extends PureStackOpCode(117, "lneg")({ case S1(x: J)  => -x  })
  case object FNeg extends PureStackOpCode(118, "fneg")({ case S1(x: F)  => -x  })
  case object DNeg extends PureStackOpCode(119, "dneg")({ case S1(x: D)  => -x  })

  case object IShl extends PureStackOpCode(120, "ishl")({ case S2(x: I, y: I)  => (y << x)  })
  case object LShl extends PureStackOpCode(121, "lshl")({ case S2(x: I, y: J)  => (y << x)  })
  case object IShr extends PureStackOpCode(122, "ishr")({ case S2(x: I, y: I)  => (y >> x)  })
  case object LShr extends PureStackOpCode(123, "lshr")({ case S2(x: I, y: J)  => (y >> x)  })

  case object IUShr extends PureStackOpCode(124, "iushr")({ case S2(x: I, y: I)  => (y >>> x)  })
  case object LUShr extends PureStackOpCode(125, "lushr")({ case S2(x: I, y: J)  => (y >>> x)  })

  case object IAnd extends PureStackOpCode(126, "iand")({ case S2(x: I, y: I)  => (x & y)  })
  case object LAnd extends PureStackOpCode(127, "land")({ case S2(x: J, y: J)  => (x & y)  })

  case object IOr extends PureStackOpCode(128, "ior")({ case S2(x: I, y: I)  => (x | y)  })
  case object LOr extends PureStackOpCode(129, "lor")({ case S2(x: J, y: J)  => (x | y)  })

  case object IXOr extends PureStackOpCode(130, "ixor")({ case S2(x: I, y: I)  => (x ^ y)  })
  case object LXOr extends PureStackOpCode(131, "lxor")({ case S2(x: J, y: J)  => (x ^ y)  })

  case class IInc(varId: Int, amount: Int) extends OpCode{
    def id = 132
    def insnName = "iinc"
    def op = vt => vt.frame.locals(varId) = (vt.frame.locals(varId).asInstanceOf[virt.Int].v) + amount
  }

  case object I2L extends PureStackOpCode(133, "i2l")({ case S1(x: I)  => x.toLong })
  case object I2F extends PureStackOpCode(134, "i2f")({ case S1(x: I)  => x.toFloat  })
  case object I2D extends PureStackOpCode(135, "i2d")({ case S1(x: I)  => x.toDouble  })

  case object L2I extends PureStackOpCode(136, "l2i")({ case S1(x: J)  => x.toInt  })
  case object L2F extends PureStackOpCode(137, "l2f")({ case S1(x: J)  => x.toFloat  })
  case object L2D extends PureStackOpCode(138, "l2d")({ case S1(x: J)  => x.toDouble  })

  case object F2I extends PureStackOpCode(139, "f2i")({ case S1(x: F)  => x.toInt  })
  case object F2L extends PureStackOpCode(140, "f2l")({ case S1(x: F)  => x.toLong  })
  case object F2D extends PureStackOpCode(141, "f2d")({ case S1(x: F)  => x.toDouble  })

  case object D2I extends PureStackOpCode(142, "d2i")({ case S1(x: D)  => x.toInt  })
  case object D2L extends PureStackOpCode(143, "d2l")({ case S1(x: D)  => x.toLong  })
  case object D2F extends PureStackOpCode(144, "d2f")({ case S1(x: D)   => x.toFloat  })

  case object I2B extends PureStackOpCode(145, "i2b")({ case S1(x: I)  => x.toByte.toInt  })
  case object I2C extends PureStackOpCode(146, "i2c")({ case S1(x: I)  => x.toChar.toInt  })
  case object I2S extends PureStackOpCode(147, "i2s")({ case S1(x: I)  => x.toShort.toInt  })

  case object LCmp extends PureStackOpCode(148, "lcmp")({ case S2(x: J, y: J)  => y.v.compare(x)  })
  case object FCmpl extends PureStackOpCode(149, "fcmpl")({ case S2(x: F, y: F)  => y.v.compare(x)  })
  case object FCmpg extends PureStackOpCode(150, "fcmpg")({ case S2(x: F, y: F)  => y.v.compare(x)  })
  case object DCmpl extends PureStackOpCode(151, "dcmpl")({ case S2(x: D, y: D)  => y.v.compare(x)  })
  case object DCmpg extends PureStackOpCode(152, "dcmpg")({ case S2(x: D, y: D)  => y.v.compare(x)  })


  abstract class UnaryBranch(val id: Byte, val insnName: String)(pred: Int => Boolean) extends OpCode{
    def label: Int
    def op = vt => {
      val virt.Int(top) = vt.pop
      if(pred(top)) vt.frame.pc = label
    }
  }

  case class IfEq(label: Int) extends UnaryBranch(153, "ifeq")(_ == 0)
  case class IfNe(label: Int) extends UnaryBranch(154, "ifne")(_ != 0)
  case class IfLt(label: Int) extends UnaryBranch(155, "iflt")(_ < 0)
  case class IfGe(label: Int) extends UnaryBranch(156, "ifge")(_ >= 0)
  case class IfGt(label: Int) extends UnaryBranch(157, "ifgt")(_ > 0)
  case class IfLe(label: Int) extends UnaryBranch(158, "ifle")(_ <= 0)

  abstract class BinaryBranch(val id: Byte, val insnName: String)(pred: (Int, Int) => Boolean) extends OpCode{
    def label: Int
    def op = vt => {
      val (virt.Int(top), virt.Int(next)) = (vt.pop, vt.pop)
      if(pred(next, top)) vt.frame.pc = label

    }
  }

  case class IfICmpEq(label: Int) extends BinaryBranch(159, "if_icmpeq")(_ == _)
  case class IfICmpNe(label: Int) extends BinaryBranch(160, "if_icmpne")(_ != _)
  case class IfICmpLt(label: Int) extends BinaryBranch(161, "if_icmplt")(_ < _)
  case class IfICmpGe(label: Int) extends BinaryBranch(162, "if_icmpge")(_ >= _)
  case class IfICmpGt(label: Int) extends BinaryBranch(163, "if_icmpgt")(_ > _)
  case class IfICmpLe(label: Int) extends BinaryBranch(164, "if_icmple")(_ <= _)
  abstract class BinaryBranchObj(val id: Byte, val insnName: String)(pred: Boolean => Boolean) extends OpCode{
    def label: Int
    def op = vt => {


      val res = (vt.pop, vt.pop) match{
        case (virt.Null, virt.Null) => true

        case (a: virt.Arr, b: virt.Arr) => a == b

        case (a: virt.Obj, b: virt.Obj) =>

          a == b
        case _ =>
          false
      }
      if(pred(res)) vt.frame.pc = label
    }
  }
  case class IfACmpEq(label: Int) extends BinaryBranchObj(165, "if_acmpeq")(x => x)
  case class IfACmpNe(label: Int) extends BinaryBranchObj(166, "if_acmpne")(x => !x)

}
