package sm
package opcodes
import scala.collection.mutable



import scala.::
import java.util
import vrt.{Cat1, Cat2}

object StackManip {
  class PureStackOpCode(transform: PartialFunction[mutable.ArrayStack[vrt.StackVal], vrt.StackVal]) extends OpCode{
    def op(vt: VmThread) =  vt.push(transform(vt.frame.stack))
  }
  object S2{
    def unapply(x: mutable.ArrayStack[vrt.StackVal]) = Some((x.pop, x.pop))
  }
  object S1{
    def unapply(x: mutable.ArrayStack[vrt.StackVal]) = Some((x.pop))
  }

  class ManipOpCode(transform: List[vrt.StackVal] => List[vrt.StackVal]) extends OpCode{
    def op(vt: VmThread) =  {
      val in = List.fill(vt.frame.stack.length min 4)(vt.pop)
      val out = transform(in)
      out.reverseMap(vt.push)
    }
  }
  case object Pop extends ManipOpCode({ case _ :: s => s })
  case object Pop2 extends ManipOpCode({
    case (_: Cat1) :: (_: Cat1) :: s => s
    case (_: Cat2) :: s => s
  })
  case object Dup extends ManipOpCode({ case top :: s => top :: top :: s })
  case object DupX1 extends ManipOpCode({ case top :: x :: s => top :: x :: top :: s })
  case object DupX2 extends ManipOpCode({
    case (top: Cat1) :: (y: Cat1) :: (x: Cat1) :: s => top :: y :: x :: top :: s
    case (top: Cat1) :: (x: Cat2) :: s => top :: x :: top :: s
  })
  case object Dup2 extends ManipOpCode({
    case (y: Cat1) :: (x: Cat1) :: s => y :: x :: y :: x :: s
    case (x: Cat2) :: s => x :: x :: s
  })
  case object Dup2X1 extends ManipOpCode({
    case (a: Cat1) :: (b: Cat1) :: (x: Cat1) :: s => a :: b :: x :: a :: b :: s
    case (a: Cat2) :: (x: Cat1) :: s => a :: a :: x :: a :: s
  })
  case object Dup2X2 extends ManipOpCode({
    case (a: Cat1) :: (b: Cat1) :: (x: Cat1) :: (y: Cat1) :: s => a :: b :: x :: y :: a :: b :: s
    case (a: Cat2) :: (b: Cat1) :: (x: Cat1) :: s => a :: b :: x :: a :: s
    case (a: Cat1) :: (b: Cat1) :: (x: Cat2) :: s => a :: b :: x :: a :: b :: s
    case (a: Cat2) :: (b: Cat2) :: s => a :: b :: a :: s
  })
  case object Swap extends ManipOpCode({ case x :: y :: s=> y :: x :: s })


  case object IAdd extends PureStackOpCode({ case S2(x: I, y: I) => x.v + y})
  case object LAdd extends PureStackOpCode({ case S2(x: J, y: J) => y.v + x})
  case object FAdd extends PureStackOpCode({ case S2(x: F, y: F)  => y.v + x})
  case object DAdd extends PureStackOpCode({ case S2(x: D, y: D)  => y.v + x})

  case object ISub extends PureStackOpCode({ case S2(x: I, y: I)  => (y - x) })
  case object LSub extends PureStackOpCode({ case S2(x: J, y: J)  => (y - x) })
  case object FSub extends PureStackOpCode({ case S2(x: F, y: F)  => (y - x) })
  case object DSub extends PureStackOpCode({ case S2(x: D, y: D)  => (y - x) })

  case object IMul extends PureStackOpCode({ case S2(x: I, y: I)  => (y * x) })
  case object LMul extends PureStackOpCode({ case S2(x: J, y: J)  => (y * x) })
  case object FMul extends PureStackOpCode({ case S2(x: F, y: F)  => (y * x) })
  case object DMul extends PureStackOpCode({ case S2(x: D, y: D)  => (y * x) })

  case object IDiv extends PureStackOpCode({ case S2(x: I, y: I)  => (y / x) })
  case object LDiv extends PureStackOpCode({ case S2(x: J, y: J)  => (y / x) })
  case object FDiv extends PureStackOpCode({ case S2(x: F, y: F)  => (y / x) })
  case object DDiv extends PureStackOpCode({ case S2(x: D, y: D)  => (y / x) })

  case object IRem extends PureStackOpCode({ case S2(x: I, y: I)  => (y % x) })
  case object LRem extends PureStackOpCode({ case S2(x: J, y: J)  => (y % x) })
  case object FRem extends PureStackOpCode({ case S2(x: F, y: F)  => (y % x) })
  case object DRem extends PureStackOpCode({ case S2(x: D, y: D)  => (y % x) })

  case object INeg extends PureStackOpCode({ case S1(x: I)  => -x  })
  case object LNeg extends PureStackOpCode({ case S1(x: J)  => -x  })
  case object FNeg extends PureStackOpCode({ case S1(x: F)  => -x  })
  case object DNeg extends PureStackOpCode({ case S1(x: D)  => -x  })

  case object IShl extends PureStackOpCode({ case S2(x: I, y: I)  => (y << x)  })
  case object LShl extends PureStackOpCode({ case S2(x: I, y: J)  => (y << x)  })
  case object IShr extends PureStackOpCode({ case S2(x: I, y: I)  => (y >> x)  })
  case object LShr extends PureStackOpCode({ case S2(x: I, y: J)  => (y >> x)  })

  case object IUShr extends PureStackOpCode({ case S2(x: I, y: I)  => (y >>> x)  })
  case object LUShr extends PureStackOpCode({ case S2(x: I, y: J)  => (y >>> x)  })

  case object IAnd extends PureStackOpCode({ case S2(x: I, y: I)  => (x & y)  })
  case object LAnd extends PureStackOpCode({ case S2(x: J, y: J)  => (x & y)  })

  case object IOr extends PureStackOpCode({ case S2(x: I, y: I)  => (x | y)  })
  case object LOr extends PureStackOpCode({ case S2(x: J, y: J)  => (x | y)  })

  case object IXOr extends PureStackOpCode({ case S2(x: I, y: I)  => (x ^ y)  })
  case object LXOr extends PureStackOpCode({ case S2(x: J, y: J)  => (x ^ y)  })

  case class IInc(varId: Int, amount: Int) extends OpCode{
    def id = 132
    def insnName = "iinc"
    def op(vt: VmThread) =  vt.frame.locals(varId) = (vt.frame.locals(varId).asInstanceOf[vrt.Int].v) + amount
  }

  case object I2L extends PureStackOpCode({ case S1(x: I)  => x.toLong })
  case object I2F extends PureStackOpCode({ case S1(x: I)  => x.toFloat  })
  case object I2D extends PureStackOpCode({ case S1(x: I)  => x.toDouble  })

  case object L2I extends PureStackOpCode({ case S1(x: J)  => x.toInt  })
  case object L2F extends PureStackOpCode({ case S1(x: J)  => x.toFloat  })
  case object L2D extends PureStackOpCode({ case S1(x: J)  => x.toDouble  })

  case object F2I extends PureStackOpCode({ case S1(x: F)  => x.toInt  })
  case object F2L extends PureStackOpCode({ case S1(x: F)  => x.toLong  })
  case object F2D extends PureStackOpCode({ case S1(x: F)  => x.toDouble  })

  case object D2I extends PureStackOpCode({ case S1(x: D)  => x.toInt  })
  case object D2L extends PureStackOpCode({ case S1(x: D)  => x.toLong  })
  case object D2F extends PureStackOpCode({ case S1(x: D)   => x.toFloat  })

  case object I2B extends PureStackOpCode({ case S1(x: I)  => x.toByte.toInt  })
  case object I2C extends PureStackOpCode({ case S1(x: I)  => x.toChar.toInt  })
  case object I2S extends PureStackOpCode({ case S1(x: I)  => x.toShort.toInt  })

  case object LCmp extends PureStackOpCode({ case S2(x: J, y: J)  => y.v.compare(x)  })
  case object FCmpl extends PureStackOpCode({ case S2(x: F, y: F)  => y.v.compare(x)  })
  case object FCmpg extends PureStackOpCode({ case S2(x: F, y: F)  => y.v.compare(x)  })
  case object DCmpl extends PureStackOpCode({ case S2(x: D, y: D)  => y.v.compare(x)  })
  case object DCmpg extends PureStackOpCode({ case S2(x: D, y: D)  => y.v.compare(x)  })


  abstract class UnaryBranch(pred: Int => Boolean) extends OpCode{
    def label: Int
    def op(vt: VmThread) =  {
      val vrt.Int(top) = vt.pop
      if(pred(top)) vt.frame.pc = label
    }
  }

  case class IfEq(label: Int) extends UnaryBranch(_ == 0)
  case class IfNe(label: Int) extends UnaryBranch(_ != 0)
  case class IfLt(label: Int) extends UnaryBranch(_ < 0)
  case class IfGe(label: Int) extends UnaryBranch(_ >= 0)
  case class IfGt(label: Int) extends UnaryBranch(_ > 0)
  case class IfLe(label: Int) extends UnaryBranch(_ <= 0)

  abstract class BinaryBranch(pred: (Int, Int) => Boolean) extends OpCode{
    def label: Int
    def op(vt: VmThread) =  {
      val (vrt.Int(top), vrt.Int(next)) = (vt.pop, vt.pop)
      if(pred(next, top)) vt.frame.pc = label

    }
  }

  case class IfICmpEq(label: Int) extends BinaryBranch(_ == _)
  case class IfICmpNe(label: Int) extends BinaryBranch(_ != _)
  case class IfICmpLt(label: Int) extends BinaryBranch(_ < _)
  case class IfICmpGe(label: Int) extends BinaryBranch(_ >= _)
  case class IfICmpGt(label: Int) extends BinaryBranch(_ > _)
  case class IfICmpLe(label: Int) extends BinaryBranch(_ <= _)
  abstract class BinaryBranchObj(pred: Boolean => Boolean) extends OpCode{
    def label: Int
    def op(vt: VmThread) = {


      val res = (vt.pop, vt.pop) match{
        case (vrt.Null, vrt.Null) => true

        case (a: vrt.Arr, b: vrt.Arr) => a == b

        case (a: vrt.Obj, b: vrt.Obj) =>

          a == b
        case _ =>
          false
      }
      if(pred(res)) vt.frame.pc = label
    }
  }
  case class IfACmpEq(label: Int) extends BinaryBranchObj(x => x)
  case class IfACmpNe(label: Int) extends BinaryBranchObj(x => !x)

}
