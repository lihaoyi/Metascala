package metascala
package opcodes
import scala.collection.mutable



import scala.::
import java.util
import rt.Thread

object StackManip {

  case class ManipStack(override val toString: String)(transform: List[Val] => List[Val]) extends OpCode{
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

  val Pop = ManipStack("Pop"){ case _ :: s => s }
  val Pop2 = ManipStack("Pop2"){ case _ :: _ :: s => s }
  val Dup = ManipStack("Dup"){ case top :: s => top :: top :: s }
  val DupX1 = ManipStack("DupX1"){ case top :: x :: s => top :: x :: top :: s }
  val DupX2 = ManipStack("DupX2"){ case top :: y :: x :: s => top :: y :: x :: top :: s }
  val Dup2 = ManipStack("Dup2"){ case y :: x :: s => y :: x :: y :: x :: s }
  val Dup2X1 = ManipStack("Dup2X1"){ case a :: b :: x :: s => a :: b :: x :: a :: b :: s }
  val Dup2X2 = ManipStack("Dup2X2"){ case a :: b :: x :: y :: s => a :: b :: x :: y :: a :: b :: s }
  val Swap = ManipStack("Swap"){ case x :: y :: s=> y :: x :: s }


  val IAdd = BinOp("IAdd")(I, I, I)(_ + _)
  val LAdd = BinOp("LAdd")(J, J, J)(_ + _)
  val FAdd = BinOp("FAdd")(F, F, F)(_ + _)
  val DAdd = BinOp("DAdd")(D, D, D)(_ + _)

  val ISub = BinOp("ISub")(I, I, I)(_ - _)
  val LSub = BinOp("LSub")(J, J, J)(_ - _)
  val FSub = BinOp("FSub")(F, F, F)(_ - _)
  val DSub = BinOp("DSub")(D, D, D)(_ - _)

  val IMul = BinOp("IMul")(I, I, I)(_ * _)
  val LMul = BinOp("LMul")(J, J, J)(_ * _)
  val FMul = BinOp("FMul")(F, F, F)(_ * _)
  val DMul = BinOp("DMul")(D, D, D)(_ * _)

  val IDiv = BinOp("IDiv")(I, I, I)(_ / _)
  val LDiv = BinOp("LDiv")(J, J, J)(_ / _)
  val FDiv = BinOp("FDiv")(F, F, F)(_ / _)
  val DDiv = BinOp("DDiv")(D, D, D)(_ / _)

  val IRem = BinOp("IRem")(I, I, I)(_ % _)
  val LRem = BinOp("LRem")(J, J, J)(_ % _)
  val FRem = BinOp("FRem")(F, F, F)(_ % _)
  val DRem = BinOp("DRem")(D, D, D)(_ % _)

  val INeg = UnaryOp("INeg")(I, I)(-_)
  val LNeg = UnaryOp("LNeg")(J, J)(-_)
  val FNeg = UnaryOp("FNeg")(F, F)(-_)
  val DNeg = UnaryOp("DNeg")(D, D)(-_)

  val IShl = BinOp("IShl")(I, I, I)(_ << _)
  val LShl = BinOp("LShl")(I, J, J)(_ << _)
  val IShr = BinOp("IShr")(I, I, I)(_ >> _)
  val LShr = BinOp("LShr")(I, J, J)(_ >> _)

  val IUShr = BinOp("IUShr")(I, I, I)(_ >>> _)
  val LUShr = BinOp("LUShr")(I, J, J)(_ >>> _)

  val IAnd = BinOp("IAnd")(I, I, I)(_ & _)
  val LAnd = BinOp("LAnd")(J, J, J)(_ & _)

  val IOr = BinOp("IOr")(I, I, I)(_ | _)
  val LOr = BinOp("LOr")(J, J, J)(_ | _)

  val IXOr = BinOp("IXOr")(I, I, I)(_ ^ _)
  val LXOr = BinOp("LXOr")(J, J, J)(_ ^ _)

  case class IInc(varId: Int, amount: Int) extends OpCode{
    def op(vt: Thread) =  vt.frame.locals(varId) = (vt.frame.locals(varId)) + amount
  }

  case class UnaryOp[A, R](override val toString: String)
                     (a: Prim[A], out: Prim[R])
                     (func: A => R) extends OpCode{
    def op(vt: Thread) = {
      val top = vt.popArgs(a.size)
      val x = func(a.read(reader(top, 0)))
      out.write(x, vt.push)
    }
  }
  case class BinOp[A, B, R](override val toString: String)
                      (a: Prim[A], b: Prim[B], out: Prim[R])
                      (func: (B, A) => R) extends OpCode{
    def op(vt: Thread) = {
      val top = vt.popArgs(a.size + b.size)
      val first = a.read(reader(top, b.size))
      val second = b.read(reader(top, 0))
      val res = func(second, first)
      out.write(res, vt.push)
    }
  }
  val I2L = UnaryOp("I2L")(I, J)(_.toLong)
  val I2F = UnaryOp("I2F")(I, F)(_.toFloat)
  val I2D = UnaryOp("I2D")(I, D)(_.toDouble)

  val L2I = UnaryOp("L2I")(J, I)(_.toInt)
  val L2F = UnaryOp("L2F")(J, F)(_.toFloat)
  val L2D = UnaryOp("L2D")(J, D)(_.toDouble)

  val F2I = UnaryOp("F2I")(F, I)(_.toInt)
  val F2L = UnaryOp("F2L")(F, J)(_.toLong)
  val F2D = UnaryOp("F2D")(F, D)(_.toDouble)

  val D2I = UnaryOp("D2I")(D, I)(_.toInt)
  val D2L = UnaryOp("D2L")(D, F)(_.toLong)
  val D2F = UnaryOp("D2F")(D, F)(_.toFloat)

  val I2B = UnaryOp("I2B")(I, B)(_.toByte)
  val I2C = UnaryOp("I2C")(I, C)(_.toChar)
  val I2S = UnaryOp("I2S")(I, S)(_.toShort)

  val LCmp = BinOp("LCmp")(J, J, I)(_ compare _)
  val FCmpl = BinOp("FCmpl")(F, F, I)(_ compare _)
  val FCmpg = BinOp("FCmpg")(F, F, I)(_ compare _)
  val DCmpl = BinOp("DCmpl")(D, D, I)(_ compare _)
  val DCmpg = BinOp("DCmpG")(D, D, I)(_ compare _)


  case class UnaryBranch(override val toString: String)
                        (label: Int, pred: Int => Boolean) extends OpCode{

    def op(vt: Thread) =  {
      if(pred(vt.pop)) vt.frame.pc = label
    }
  }

  val IfEq = UnaryBranch("IfEq")(_: Int, _ == 0)
  val IfNe = UnaryBranch("IfNe")(_: Int, _ != 0)
  val IfLt = UnaryBranch("IfLt")(_: Int, _ < 0)
  val IfGe = UnaryBranch("IfGe")(_: Int, _ >= 0)
  val IfGt = UnaryBranch("IfGt")(_: Int, _ > 0)
  val IfLe = UnaryBranch("IfLe")(_: Int, _ <= 0)

  case class BinaryBranch(override val toString: String)
                         (label: Int, pred: (Int, Int) => Boolean) extends OpCode{

    def op(vt: Thread) =  {
      val (a, b) = (vt.pop, vt.pop)
      if(pred(b, a)) vt.frame.pc = label
    }
  }

  val IfICmpEq = BinaryBranch("IfICmpEq")(_: Int, _ == _)
  val IfICmpNe = BinaryBranch("IfICmpNe")(_: Int, _ != _)
  val IfICmpLt = BinaryBranch("IfICmpLt")(_: Int, _ < _)
  val IfICmpGe = BinaryBranch("IfICmpGe")(_: Int, _ >= _)
  val IfICmpGt = BinaryBranch("IfICmpGt")(_: Int, _ > _)
  val IfICmpLe = BinaryBranch("IfICmpLe")(_: Int, _ <= _)

  case class BinaryBranchObj(override val toString: String)
                            (label: Int, pred: Boolean => Boolean) extends OpCode{
    def op(vt: Thread) = {

      val res = (vt.pop, vt.pop) match{
        case (0, 0) => true
        case (a, b) => a == b
        case _ => false
      }
      if(pred(res)) vt.frame.pc = label
    }
  }
  val IfACmpEq= BinaryBranchObj("IfACmpEq")(_: Int, x => x)
  val IfACmpNe= BinaryBranchObj("IfACmpNe")(_: Int, x => !x)

}
