package svm.model.opcodes

import svm.model.TypeDesc._
import svm.model.{OpCode, a}
import svm.Virtualizer

import scala.::
import java.util

object  StackManip {
  class PureStackOpCode(val id: Byte, val insnName: String)(transform: List[Any] => List[Any]) extends OpCode{
    def op = ctx => ctx.frame.stack = transform(ctx.stack)
  }
  case object Pop extends PureStackOpCode(87, "pop")({ case _ :: s => s })
  case object Pop2 extends PureStackOpCode(88, "pop2")({
    case Cat1(_) :: Cat1(_) :: s => s
    case Cat2(_) :: s => s
  })
  case object Dup extends PureStackOpCode(89, "dup")({ case top :: s => top :: top :: s })
  case object DupX1 extends PureStackOpCode(90, "dup_x1")({ case top :: x :: s => top :: x :: top :: s })
  case object DupX2 extends PureStackOpCode(91, "dup_x2")({
    case Cat1(top) :: Cat1(y) :: Cat1(x) :: s => top :: y :: x :: top :: s
    case Cat1(top) :: Cat2(x) :: s => top :: x :: top :: s
  })
  case object Dup2 extends PureStackOpCode(92, "dup2")({
    case Cat1(y) :: Cat1(x) :: s => y :: x :: y :: x :: s
    case Cat2(x) :: s => x :: x :: s
  })
  case object Dup2X1 extends PureStackOpCode(93, "dup2_x1")({
    case Cat1(a) :: Cat1(b) :: Cat1(x) :: s => a :: b :: x :: a :: b :: s
    case Cat2(a) :: Cat1(x) :: s => a :: a :: x :: a :: s
  })
  case object Dup2X2 extends PureStackOpCode(94, "dup2_x2")({
    case Cat1(a) :: Cat1(b) :: Cat1(x) :: Cat1(y) :: s => a :: b :: x :: y :: a :: b :: s
    case Cat2(a) :: Cat1(b) :: Cat1(x) :: s => a :: b :: x :: a :: s
    case Cat1(a) :: Cat1(b) :: Cat2(x) :: s => a :: b :: x :: a :: b :: s
    case Cat2(a) :: Cat2(b) :: s => a :: b :: a :: s
  })
  case object Swap extends PureStackOpCode(95, "swap")({ case x :: y :: s=> y :: x :: s })

  case object IAdd extends PureStackOpCode(96, "iadd")({ case (x: I) :: (y: I) :: s => (x + y) :: s })
  case object LAdd extends PureStackOpCode(97, "ladd")({ case (x: J) :: (y: J) :: s => (y + x) :: s})
  case object FAdd extends PureStackOpCode(98, "fadd")({ case (x: F) :: (y: F) :: s => (y + x) :: s})
  case object DAdd extends PureStackOpCode(99, "dadd")({ case (x: D) :: (y: D) :: s => (y + x) :: s})

  case object ISub extends PureStackOpCode(100, "isub")({ case (x: I) :: (y: I) :: s => (y - x) :: s})
  case object LSub extends PureStackOpCode(101, "lsub")({ case (x: J) :: (y: J) :: s => (y - x) :: s})
  case object FSub extends PureStackOpCode(102, "fsub")({ case (x: F) :: (y: F) :: s => (y - x) :: s})
  case object DSub extends PureStackOpCode(103, "dsub")({ case (x: D) :: (y: D) :: s => (y - x) :: s})

  case object IMul extends PureStackOpCode(104, "imul")({ case (x: I) :: (y: I) :: s => (y * x) :: s})
  case object LMul extends PureStackOpCode(105, "lmul")({ case (x: J) :: (y: J) :: s => (y * x) :: s})
  case object FMul extends PureStackOpCode(106, "fmul")({ case (x: F) :: (y: F) :: s => (y * x) :: s})
  case object DMul extends PureStackOpCode(107, "dmul")({ case (x: D) :: (y: D) :: s => (y * x) :: s})

  case object IDiv extends PureStackOpCode(108, "idiv")({ case (x: I) :: (y: I) :: s => (y / x) :: s})
  case object LDiv extends PureStackOpCode(109, "ldiv")({ case (x: J) :: (y: J) :: s => (y / x) :: s})
  case object FDiv extends PureStackOpCode(110, "fdiv")({ case (x: F) :: (y: F) :: s => (y / x) :: s})
  case object DDiv extends PureStackOpCode(111, "ddiv")({ case (x: D) :: (y: D) :: s => (y / x) :: s})

  case object IRem extends PureStackOpCode(112, "irem")({ case (x: I) :: (y: I) :: s => (y % x) :: s})
  case object LRem extends PureStackOpCode(113, "lrem")({ case (x: J) :: (y: J) :: s => (y % x) :: s})
  case object FRem extends PureStackOpCode(114, "frem")({ case (x: F) :: (y: F) :: s => (y % x) :: s})
  case object DRem extends PureStackOpCode(115, "drem")({ case (x: D) :: (y: D) :: s => (y % x) :: s})

  case object INeg extends PureStackOpCode(116, "ineg")({ case (x: I) :: s => -x :: s })
  case object LNeg extends PureStackOpCode(117, "lneg")({ case (x: J) :: s => -x :: s })
  case object FNeg extends PureStackOpCode(118, "fneg")({ case (x: F) :: s => -x :: s })
  case object DNeg extends PureStackOpCode(119, "dneg")({ case (x: D) :: s => -x :: s })

  case object IShl extends PureStackOpCode(120, "ishl")({ case (x: I) :: (y: I) :: s => (y << x) :: s })
  case object LShl extends PureStackOpCode(121, "lshl")({ case (x: I) :: (y: J) :: s => (y << x) :: s })
  case object IShr extends PureStackOpCode(122, "ishr")({ case (x: I) :: (y: I) :: s => (y >> x) :: s })
  case object LShr extends PureStackOpCode(123, "lshr")({ case (x: I) :: (y: J) :: s => (y >> x) :: s })

  case object IUShr extends PureStackOpCode(124, "iushr")({ case (x: I) :: (y: I) :: s => (y >>> x) :: s })
  case object LUShr extends PureStackOpCode(125, "lushr")({ case (x: I) :: (y: J) :: s => (y >>> x) :: s })

  case object IAnd extends PureStackOpCode(126, "iand")({ case (x: I) :: (y: I) :: s => (x & y) :: s })
  case object LAnd extends PureStackOpCode(127, "land")({ case (x: J) :: (y: J) :: s => (x & y) :: s })

  case object IOr extends PureStackOpCode(128, "ior")({ case (x: I) :: (y: I) :: s => (x | y) :: s })
  case object LOr extends PureStackOpCode(129, "lor")({ case (x: J) :: (y: J) :: s => (x | y) :: s })

  case object IXOr extends PureStackOpCode(130, "ixor")({ case (x: I) :: (y: I) :: s => (x ^ y) :: s })
  case object LXOr extends PureStackOpCode(131, "lxor")({ case (x: J) :: (y: J) :: s => (x ^ y) :: s })

  case class IInc(varId: Int, amount: Int) extends OpCode{
    def id = 132
    def insnName = "iinc"
    def op = ctx => ctx.frame.locals(varId) = (ctx.frame.locals(varId).asInstanceOf[Int]) + amount
  }

  case object I2L extends PureStackOpCode(133, "i2l")({ case (x: I) :: s => x.toLong :: s})
  case object I2F extends PureStackOpCode(134, "i2f")({ case (x: I) :: s => x.toFloat :: s })
  case object I2D extends PureStackOpCode(135, "i2d")({ case (x: I) :: s => x.toDouble :: s })

  case object L2I extends PureStackOpCode(136, "l2i")({ case (x: J) :: s => x.toInt :: s })
  case object L2F extends PureStackOpCode(137, "l2f")({ case (x: J) :: s => x.toFloat :: s })
  case object L2D extends PureStackOpCode(138, "l2d")({ case (x: J) :: s => x.toDouble :: s })

  case object F2I extends PureStackOpCode(139, "f2i")({ case (x: F) :: s => x.toInt :: s })
  case object F2L extends PureStackOpCode(140, "f2l")({ case (x: F) :: s => x.toLong :: s })
  case object F2D extends PureStackOpCode(141, "f2d")({ case (x: F) :: s => x.toDouble :: s })

  case object D2I extends PureStackOpCode(142, "d2i")({ case (x: D) :: s => x.toInt :: s })
  case object D2L extends PureStackOpCode(143, "d2l")({ case (x: D) :: s => x.toLong :: s })
  case object D2F extends PureStackOpCode(144, "d2f")({ case (x: D) :: s  => x.toFloat :: s })

  case object I2B extends PureStackOpCode(145, "i2b")({ case (x: I) :: s => x.toByte.toInt :: s })
  case object I2C extends PureStackOpCode(146, "i2c")({ case (x: I) :: s => x.toChar.toInt :: s })
  case object I2S extends PureStackOpCode(147, "i2s")({ case (x: I) :: s => x.toShort.toInt :: s })

  case object LCmp extends PureStackOpCode(148, "lcmp")({ case (x: J) :: (y: J) :: s => y.compare(x) :: s })
  case object FCmpl extends PureStackOpCode(149, "fcmpl")({ case (x: F) :: (y: F) :: s => y.compare(x) :: s })
  case object FCmpg extends PureStackOpCode(150, "fcmpg")({ case (x: F) :: (y: F) :: s => y.compare(x) :: s })
  case object DCmpl extends PureStackOpCode(151, "dcmpl")({ case (x: D) :: (y: D) :: s => y.compare(x) :: s })
  case object DCmpg extends PureStackOpCode(152, "dcmpg")({ case (x: D) :: (y: D) :: s => y.compare(x) :: s })


  abstract class UnaryBranch(val id: Byte, val insnName: String)(pred: Int => Boolean) extends OpCode{
    def label: Int
    def op = ctx => ctx.swapStack{ case Intish(top) :: stack =>
      if(pred(top)) ctx.jumpTo(label)
      stack
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
    def op = ctx => ctx.swapStack{ case Intish(top) :: Intish(next) :: stack =>
      if(pred(next, top)) ctx.jumpTo(label)
      stack
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
    def op = ctx => ctx.swapStack{ case top :: next :: stack =>
      val res = (next, top) match{
        case (null, null) => true
        case (a: Array[Byte], b: Array[Byte]) => util.Arrays.equals(a, b)
        case (a: Array[Char], b: Array[Char]) => util.Arrays.equals(a, b)
        case (a: Array[Short], b: Array[Short]) => util.Arrays.equals(a, b)
        case (a: Array[Int], b: Array[Int]) => util.Arrays.equals(a, b)
        case (a: Array[Long], b: Array[Long]) => util.Arrays.equals(a, b)
        case (a: Array[Float], b: Array[Float]) => util.Arrays.equals(a, b)
        case (a: Array[Double], b: Array[Double]) => util.Arrays.equals(a, b)
        case (a: svm.Obj, b: svm.Obj) => a == b
        case _ =>
          false
      }
      if(pred(res)) ctx.jumpTo(label)
      stack

    }
  }
  case class IfACmpEq(label: Int) extends BinaryBranchObj(165, "if_acmpeq")(x => x)
  case class IfACmpNe(label: Int) extends BinaryBranchObj(166, "if_acmpne")(x => !x)

}
