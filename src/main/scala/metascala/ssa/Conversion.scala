package metascala
package ssa

import metascala.imm.{Type, Method}
import scala.collection.mutable
import metascala.opcodes.OpCode
import metascala.opcodes.LoadStore.{Store, Ldc, Load}
import metascala.opcodes.StackManip.{BinaryBranch, UnaryBranch, UnaryOp, BinOp}
import metascala.Prim
import metascala.opcodes.Misc.{InvokeStatic, Goto, ReturnVal}

case class Symbol(n: Int){
  override def toString = ""+n
}

object Conversion {

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
      .toMap ->
    symbols.map(_.n).sum
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

  def op(state: State, op: OpCode, makeSymbol: (Int) => Symbol): (State, Option[Insn]) = op match {
    case InvokeStatic(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize)
      val target = makeSymbol(sig.desc.ret.size)
      state.copy(stack = target :: newStack) -> Some(Insn.InvokeStatic(target, args, cls, sig))

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