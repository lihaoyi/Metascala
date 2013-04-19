package metascala
package opcodes
import scala.collection.mutable



import scala.::
import java.util
import rt.Thread

object StackManip {


  class ManipOpCode(transform: List[Val] => List[Val]) extends OpCode{
    def op(vt: Thread) =  {
      var i = vt.frame.stackDump.length min 4
      var list: List[Val] = Nil
      while(i > 0){
        i -= 1
        list = vt.pop :: list
      }
      val out = transform(list.reverse)
      out.reverse.foreach(vt.push)
    }
  }

  case object Pop extends ManipOpCode({ case _ :: s => s })
  case object Pop2 extends ManipOpCode({
    case _ :: _ :: s => s

  })
  case object Dup extends ManipOpCode({ case top :: s => top :: top :: s })
  case object DupX1 extends ManipOpCode({ case top :: x :: s => top :: x :: top :: s })
  case object DupX2 extends ManipOpCode({
    case top :: y :: x :: s => top :: y :: x :: top :: s

  })
  case object Dup2 extends ManipOpCode({
    case y :: x :: s => y :: x :: y :: x :: s
  })
  case object Dup2X1 extends ManipOpCode({
    case a :: b :: x :: s => a :: b :: x :: a :: b :: s
  })
  case object Dup2X2 extends ManipOpCode({
    case a :: b :: x :: y :: s => a :: b :: x :: y :: a :: b :: s
  })
  case object Swap extends ManipOpCode({ case x :: y :: s=> y :: x :: s })

  case object IAdd extends BinOp(I, I, I)(_ + _)
  case object LAdd extends BinOp(J, J, J)(_ + _)
  case object FAdd extends BinOp(F, F, F)(_ + _)
  case object DAdd extends BinOp(D, D, D)(_ + _)

  case object ISub extends BinOp(I, I, I)(_ - _)
  case object LSub extends BinOp(J, J, J)(_ - _)
  case object FSub extends BinOp(F, F, F)(_ - _)
  case object DSub extends BinOp(D, D, D)(_ - _)

  case object IMul extends BinOp(I, I, I)(_ * _)
  case object LMul extends BinOp(J, J, J)(_ * _)
  case object FMul extends BinOp(F, F, F)(_ * _)
  case object DMul extends BinOp(D, D, D)(_ * _)

  case object IDiv extends BinOp(I, I, I)(_ / _)
  case object LDiv extends BinOp(J, J, J)(_ / _)
  case object FDiv extends BinOp(F, F, F)(_ / _)
  case object DDiv extends BinOp(D, D, D)(_ / _)

  case object IRem extends BinOp(I, I, I)(_ % _)
  case object LRem extends BinOp(J, J, J)(_ % _)
  case object FRem extends BinOp(F, F, F)(_ % _)
  case object DRem extends BinOp(D, D, D)(_ % _)

  case object INeg extends UnaryOp(I, I)(-_)
  case object LNeg extends UnaryOp(J, J)(-_)
  case object FNeg extends UnaryOp(F, F)(-_)
  case object DNeg extends UnaryOp(D, D)(-_)

  case object IShl extends BinOp(I, I, I)(_ << _)
  case object LShl extends BinOp(I, J, J)(_ << _)
  case object IShr extends BinOp(I, I, I)(_ >> _)
  case object LShr extends BinOp(I, J, J)(_ >> _)

  case object IUShr extends BinOp(I, I, I)(_ >>> _)
  case object LUShr extends BinOp(I, J, J)(_ >>> _)

  case object IAnd extends BinOp(I, I, I)(_ & _)
  case object LAnd extends BinOp(J, J, J)(_ & _)

  case object IOr extends BinOp(I, I, I)(_ | _)
  case object LOr extends BinOp(J, J, J)(_ | _)

  case object IXOr extends BinOp(I, I, I)(_ ^ _)
  case object LXOr extends BinOp(J, J, J)(_ ^ _)

  case class IInc(varId: Int, amount: Int) extends OpCode{
    def op(vt: Thread) =  vt.frame.locals(varId) = (vt.frame.locals(varId)) + amount
  }

  class UnaryOp[A, R](a: Prim[A], out: Prim[R])(func: A => R) extends OpCode{
    def op(vt: Thread) = {
      val x = func(a.read(vt.pop))
      println("UnaryOp " + x)
      out.write(x, vt.push)
    }
  }
  class BinOp[A, B, R](a: Prim[A], b: Prim[B], out: Prim[R])(func: (B, A) => R) extends OpCode{
    def op(vt: Thread) = {
      val first = a.read(vt.pop)
      val second = b.read(vt.pop)
      val res = func(second, first)
      out.write(res, vt.push)
    }
  }
  case object I2L extends UnaryOp(I, J)(_.toLong)
  case object I2F extends UnaryOp(I, F)(_.toFloat)
  case object I2D extends UnaryOp(I, D)(_.toDouble)

  case object L2I extends UnaryOp(J, I)(_.toInt)
  case object L2F extends UnaryOp(J, F)(_.toFloat)
  case object L2D extends UnaryOp(J, D)(_.toDouble)

  case object F2I extends UnaryOp(F, I)(_.toInt)
  case object F2L extends UnaryOp(F, J)(_.toLong)
  case object F2D extends UnaryOp(F, D)(_.toDouble)

  case object D2I extends UnaryOp(D, I)(_.toInt)
  case object D2L extends UnaryOp(D, F)(_.toLong)
  case object D2F extends UnaryOp(D, F)(_.toFloat)

  case object I2B extends UnaryOp(I, B)(_.toByte)
  case object I2C extends UnaryOp(I, C)(_.toChar)
  case object I2S extends UnaryOp(I, S)(_.toShort)

  case object LCmp extends BinOp(J, J, I)(_ compare _)
  case object FCmpl extends BinOp(F, F, I)(_ compare _)
  case object FCmpg extends BinOp(F, F, I)(_ compare _)
  case object DCmpl extends BinOp(D, D, I)(_ compare _)
  case object DCmpg extends BinOp(D, D, I)(_ compare _)


  abstract class UnaryBranch(pred: Int => Boolean) extends OpCode{
    def label: Int
    def op(vt: Thread) =  {
      if(pred(vt.pop)) vt.frame.pc = label
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
    def op(vt: Thread) =  {

      val (a, b) = (vt.pop, vt.pop)
      if(pred(b, a)) vt.frame.pc = label
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
    def op(vt: Thread) = {

      val res = (vt.pop, vt.pop) match{
        case (0, 0) => true
        case (a, b) => a == b
        case _ => false
      }
      if(pred(res)) vt.frame.pc = label
    }
  }
  case class IfACmpEq(label: Int) extends BinaryBranchObj(x => x)
  case class IfACmpNe(label: Int) extends BinaryBranchObj(x => !x)

}
