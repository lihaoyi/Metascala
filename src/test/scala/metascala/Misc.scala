package metascala

import org.scalatest.FreeSpec
import collection.mutable
import Gen._
import metascala.opcodes.LoadStore.{Store, Ldc, Load}
import metascala.opcodes.StackManip.{BinaryBranch, UnaryBranch, BinOp, UnaryOp}
import metascala.opcodes.Misc.{Goto, ReturnVal}
import metascala.opcodes.OpCode
import metascala.imm.Method

trait Jump{
  def phi: Seq[(Symbol, Symbol)]
  def target: Int
}

trait Insn
object Insn{
  case class BinOp(a: Symbol, b: Symbol, out: Symbol, src: OpCode) extends Insn
  case class UnaryOp(a: Symbol, out: Symbol, src: OpCode) extends Insn
  case class UnaryBranch(a: Symbol, target: Int, src: OpCode, phi: Seq[(Symbol, Symbol)] = Nil) extends Insn with Jump
  case class BinaryBranch(a: Symbol, b: Symbol, target: Int, src: OpCode, phi: Seq[(Symbol, Symbol)] = Nil) extends Insn with Jump
  case class ReturnVal(a: Symbol, n: Int) extends Insn
  case class Goto(target: Int, phi: Seq[(Symbol, Symbol)] = Nil) extends Insn with Jump
}

case class Symbol(n: Int){
  override def toString = ""+n
}

class Misc extends FreeSpec with Util{
  val arr = new Array[Int](2)
  def test[T](p: Prim[T])(cases: Iterable[T]){
    chk{ x: T =>
      p.write(x, writer(arr, 0))
      assert(p.read(reader(arr, 0)) === x)
    }(cases)
  }
  "making sure Prim[T] write & pops preserve the value T" - {
    "testZ" in test(Z)(Seq(true, false))
    "testB" in test(B)(30 ** Gen.intAll.toByte)
    "testC" in test(C)(30 ** Gen.intAll.toChar)
    "testS" in test(S)(30 ** Gen.intAll.toShort)
    "testF" in test(F)(30 ** java.lang.Float.intBitsToFloat(Gen.intAll))
    "testL" in test(J)(30 ** Gen.longAll)
    "testD" in test(D)(30 ** Gen.doubleAll)
  }

  "hello" in {
    val cls = imm.Cls.parse(natives.Bindings.default.fileLoader("metascala/features/controlflow/Loops.class").get)
    val blocks = convertToSsa(cls.methods.last)
    println(
      blocks.toSeq
            .sortBy(_._1)
            .map(_._2.map(_.toString).reduce(_+"\n"+_))
            .reduce(_+"\n\n"+_)
    )
  }


  def convertToSsa(method: Method) = {
    val insns = method.code.insns
    val symbols = mutable.Buffer[Symbol]()

    def makeSymbol(size: Int): Symbol = {
      val newSym = new Symbol(symbols.length)
      for (i <- 0 until size) symbols.append(newSym)

      symbols.last
    }

    val locals =
      method.desc.args
            .map(x => (x.size, makeSymbol(x.size)))
            .flatMap{case (size, sym) => Seq.fill(size)(sym)}
            .padTo(method.misc.maxLocals, new Symbol(-1))
            .toVector

    val (regInsns, stackToSsa, ssaToStack, states) = run(insns, locals, makeSymbol)

    val newInsns = regInsns.zipWithIndex.map{
      case (x: Jump, i) =>
        val postState = states(ssaToStack(i+1))
        val targetState = states(x.target)

        val fullZipped = (postState.locals ++ postState.stack).zip(targetState.locals ++ targetState.stack)
        val culled =
          fullZipped.distinct
                    .filter{case (a, b) => (a.n != -1) && (b.n != -1) && (a != b)}
                    .toList

        x match{
          case x: Insn.UnaryBranch   => x.copy(target = stackToSsa(x.target), phi = culled)
          case x: Insn.BinaryBranch  => x.copy(target = stackToSsa(x.target), phi = culled)
          case x: Insn.Goto          => x.copy(target = stackToSsa(x.target), phi = culled)
          case x => x
        }
      case (x, i) => x
    }

    val breaks =
      newInsns.toSeq
              .zipWithIndex
              .flatMap{case (x: Jump, i) => Seq(x.target, i); case _ => Nil}
              .distinct
              .sorted

    newInsns.zipWithIndex
            .splitAll(breaks.toList.sorted)
            .filter(_.length > 0)
            .map(s => (s(0)._2, s.map(_._1)))
            .toMap
  }
  case class State(stack: List[Symbol], locals: Vector[Symbol])

  def run(insns: Seq[OpCode], locals: Vector[Symbol], makeSymbol: Int => Symbol) = {
    val regInsns = mutable.Buffer[Insn]()
    val stackToSsa = mutable.Buffer[Int]()
    val states = mutable.Buffer[State](State(List(), locals))

    for ((insn, i) <- insns.zipWithIndex){
      stackToSsa += regInsns.length
      println(i + "\t" + insn.toString.padTo(20, ' ') + states.last.stack.toString.padTo(20, ' ') + states.last.locals.toString.padTo(30, ' '))
      val (newState, newInsn) = op(states.last, insn, makeSymbol)

      states.append(newState)
      newInsn.map{ regInsns.append(_) }
    }

    val ssaToStack =
      0.to(stackToSsa.max)
       .map(stackToSsa.drop(1).indexOf(_))
       .:+(stackToSsa.length)

    (regInsns, stackToSsa, ssaToStack, states)
  }

  def op(state: State, op: OpCode, makeSymbol: (Int) => Symbol): (State, Option[Insn]) = {
    op match {
      case Load(index, _) =>
        state.copy(stack = state.locals(index) :: state.stack) -> None

      case Ldc(thing) =>
        val size = thing match{
          case Long => 2
          case Double => 2
          case _ => 1
        }
       state.copy(stack = makeSymbol(size) :: state.stack) -> None

      case BinOp(_, _, Prim(size)) =>
        val symbol = makeSymbol(size)
        val first :: second :: newStack = state.stack
        state.copy(stack = symbol :: newStack) -> Some(Insn.BinOp(first, second, symbol, op))

      case UnaryOp(_, Prim(size)) =>
        val symbol = makeSymbol(size)
        val first :: newStack = state.stack

        state.copy(stack = symbol :: newStack) -> Some(Insn.UnaryOp(first, symbol, op))
      case Store(index, Prim(size)) =>
        state.copy(stack = state.stack.tail, locals = state.locals.patch(index, Seq.fill(size)(state.stack.head), size)) -> None

      case UnaryBranch(index) =>
        state.copy(stack = state.stack.tail) -> Some(Insn.UnaryBranch(state.stack.head, index, op))

      case BinaryBranch(index) =>
        val first :: second :: newStack = state.stack
        state.copy(stack = newStack) -> Some(Insn.BinaryBranch(first, second, index, op))

      case ReturnVal(n) =>
        state.copy(stack = state.stack.tail) -> Some(Insn.ReturnVal(state.stack.head, n))

      case Goto(index) =>
        state -> Some(Insn.Goto(index))
    }
  }


  implicit class splitAllable[T](c: Seq[T]){
    def splitAll(positions: List[Int], index: Int = 0): Seq[Seq[T]] = positions match{
      case Nil => Seq(c)
      case firstPos :: restPos =>
        val (first, rest) = c.splitAt(firstPos - index)
        first +: rest.splitAll(restPos, firstPos)
    }
  }

}
