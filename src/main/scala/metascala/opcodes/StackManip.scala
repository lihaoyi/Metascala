package metascala
package opcodes
import scala.collection.mutable



import scala.::
import java.util
import rt.Thread

trait StackManip {

  case class ManipStack(transform: List[Any] => List[Any])(override val toString: String) extends OpCode
  val Pop = ManipStack{ case _ :: s => s }("Pop")
  val Pop2 = ManipStack{ case _ :: _ :: s => s }("Pop2")
  val Dup = ManipStack{ case top :: s => top :: top :: s }("Dup")
  val DupX1 = ManipStack{ case top :: x :: s => top :: x :: top :: s }("DupX1")
  val DupX2 = ManipStack{ case top :: y :: x :: s => top :: y :: x :: top :: s }("DupX2")
  val Dup2 = ManipStack{ case y :: x :: s => y :: x :: y :: x :: s }("Dup2")
  val Dup2X1 = ManipStack{ case a :: b :: x :: s => a :: b :: x :: a :: b :: s }("Dup2X1")
  val Dup2X2 = ManipStack{ case a :: b :: x :: y :: s => a :: b :: x :: y :: a :: b :: s }("Dup2X2")
  val Swap = ManipStack{ case x :: y :: s=> y :: x :: s }("Swap")


  val IAdd = BinOp(I, I, I)(_ + _)("IAdd")
  val LAdd = BinOp(J, J, J)(_ + _)("LAdd")
  val FAdd = BinOp(F, F, F)(_ + _)("FAdd")
  val DAdd = BinOp(D, D, D)(_ + _)("DAdd")

  val ISub = BinOp(I, I, I)(_ - _)("ISub")
  val LSub = BinOp(J, J, J)(_ - _)("LSub")
  val FSub = BinOp(F, F, F)(_ - _)("FSub")
  val DSub = BinOp(D, D, D)(_ - _)("DSub")

  val IMul = BinOp(I, I, I)(_ * _)("IMul")
  val LMul = BinOp(J, J, J)(_ * _)("LMul")
  val FMul = BinOp(F, F, F)(_ * _)("FMul")
  val DMul = BinOp(D, D, D)(_ * _)("DMul")

  val IDiv = BinOp(I, I, I)(_ / _)("IDiv")
  val LDiv = BinOp(J, J, J)(_ / _)("LDiv")
  val FDiv = BinOp(F, F, F)(_ / _)("FDiv")
  val DDiv = BinOp(D, D, D)(_ / _)("DDiv")

  val IRem = BinOp(I, I, I)(_ % _)("IRem")
  val LRem = BinOp(J, J, J)(_ % _)("LRem")
  val FRem = BinOp(F, F, F)(_ % _)("FRem")
  val DRem = BinOp(D, D, D)(_ % _)("DRem")

  val INeg = UnaryOp(I, I)(-_)("INeg")
  val LNeg = UnaryOp(J, J)(-_)("LNeg")
  val FNeg = UnaryOp(F, F)(-_)("FNeg")
  val DNeg = UnaryOp(D, D)(-_)("DNeg")

  val IShl = BinOp(I, I, I)(_ << _)("IShl")
  val LShl = BinOp(I, J, J)(_ << _)("LShl")
  val IShr = BinOp(I, I, I)(_ >> _)("IShr")
  val LShr = BinOp(I, J, J)(_ >> _)("LShr")

  val IUShr = BinOp(I, I, I)(_ >>> _)("IUShr")
  val LUShr = BinOp(I, J, J)(_ >>> _)("LUShr")

  val IAnd = BinOp(I, I, I)(_ & _)("IAnd")
  val LAnd = BinOp(J, J, J)(_ & _)("LAnd")

  val IOr = BinOp(I, I, I)(_ | _)("IOr")
  val LOr = BinOp(J, J, J)(_ | _)("LOr")

  val IXOr = BinOp(I, I, I)(_ ^ _)("IXOr")
  val LXOr = BinOp(J, J, J)(_ ^ _)("LXOr")

  case class IInc(varId: Int, amount: Int) extends OpCode

  case class UnaryOp[A, R](a: Prim[A], out: Prim[R])
                          (val func: A => R)(override val toString: String) extends OpCode

  case class BinOp[A, B, R](a: Prim[A], b: Prim[B], out: Prim[R])
                           (val func: (B, A) => R)
                           (override val toString: String)extends OpCode

  val I2L = UnaryOp(I, J)(_.toLong)  ("I2L")
  val I2F = UnaryOp(I, F)(_.toFloat) ("I2F")
  val I2D = UnaryOp(I, D)(_.toDouble)("I2D")

  val L2I = UnaryOp(J, I)(_.toInt)   ("L2I")
  val L2F = UnaryOp(J, F)(_.toFloat) ("L2F")
  val L2D = UnaryOp(J, D)(_.toDouble)("L2D")

  val F2I = UnaryOp(F, I)(_.toInt)   ("F2I")
  val F2L = UnaryOp(F, J)(_.toLong)  ("F2L")
  val F2D = UnaryOp(F, D)(_.toDouble)("F2D")

  val D2I = UnaryOp(D, I)(_.toInt)   ("D2I")
  val D2L = UnaryOp(D, F)(_.toLong)  ("D2L")
  val D2F = UnaryOp(D, F)(_.toFloat) ("D2F")

  val I2B = UnaryOp(I, B)(_.toByte)  ("I2B")
  val I2C = UnaryOp(I, C)(_.toChar)  ("I2C")
  val I2S = UnaryOp(I, S)(_.toShort) ("I2S")

  val LCmp = BinOp(J, J, I)(_ compare _)("LCmp")
  val FCmpl = BinOp(F, F, I)(_ compare _)("FCmpl")
  val FCmpg = BinOp(F, F, I)(_ compare _)("FCmpg")
  val DCmpl = BinOp(D, D, I)(_ compare _)("DCmpl")
  val DCmpg = BinOp(D, D, I)(_ compare _)("DCmpG")


  case class UnaryBranch(label: Int)
                        (val pred: Int => Boolean)
                        (name: String)extends OpCode

  val IfEq = UnaryBranch(_: Int)(_ == 0)("IfEq")
  val IfNe = UnaryBranch(_: Int)(_ != 0)("IfNe")
  val IfLt = UnaryBranch(_: Int)(_ < 0) ("IfLt")
  val IfGe = UnaryBranch(_: Int)(_ >= 0)("IfGe")
  val IfGt = UnaryBranch(_: Int)(_ > 0) ("IfGt")
  val IfLe = UnaryBranch(_: Int)(_ <= 0)("IfLe")

  case class BinaryBranch(label: Int)
                         (val pred: (Int, Int) => Boolean)
                         (name: String) extends OpCode

  val IfICmpEq = BinaryBranch(_: Int)(_ == _)("IfICmpEq")
  val IfICmpNe = BinaryBranch(_: Int)(_ != _)("IfICmpNe")
  val IfICmpLt = BinaryBranch(_: Int)(_ < _) ("IfICmpLt")
  val IfICmpGe = BinaryBranch(_: Int)(_ >= _)("IfICmpGe")
  val IfICmpGt = BinaryBranch(_: Int)(_ > _) ("IfICmpGt")
  val IfICmpLe = BinaryBranch(_: Int)(_ <= _)("IfICmpLe")

  val IfACmpEq= BinaryBranch(_: Int)(_ == _) ("IfACmpEq")
  val IfACmpNe= BinaryBranch(_: Int)(_ != _)("IfACmpNe")

}
