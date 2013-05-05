package metascala
package ssa

import metascala.imm.{Type, Method}
import scala.collection.mutable
import metascala.opcodes.OpCode

import metascala.Prim
import metascala.opcodes.Misc._

import metascala.opcodes.LoadStore.Ldc
import metascala.opcodes.LoadStore.Load
import metascala.opcodes.LoadStore.Push
import metascala.opcodes.StackManip.UnaryBranch
import scala.Some
import metascala.opcodes.Misc.InvokeStatic
import metascala.opcodes.Misc.ReturnVal
import metascala.ssa.Symbol
import metascala.opcodes.LoadStore.Store
import metascala.opcodes.StackManip.BinaryBranch
import metascala.opcodes.StackManip.BinOp
import metascala.opcodes.LoadStore.Const
import metascala.opcodes.Misc.Goto
import metascala.opcodes.StackManip.BinaryBranchObj
import metascala.opcodes.StackManip.UnaryOp

case class Symbol[T](n: Int, prim: Prim[T]){
  override def toString = ""+n
  def size = prim.size
}

object Conversion {
  def convertToSsa(method: Method)(implicit vt: rt.Thread): (Map[Int, Seq[Insn]], Int) = {
    println("Converting: " + method)
    val insns = method.code.insns
    if (insns.isEmpty) {
      Map() -> 0
    } else {
      val symbols = mutable.Buffer[Symbol[_]]()

      def makeSymbol[T](prim: Prim[T]): Symbol[T] = {
        println("MAKE SYMBOL " + prim + " " + prim.size)
        val newSym = new Symbol(symbols.length, prim)
        for (i <- 0 until prim.size) symbols.append(newSym)

        newSym
      }

      val locals: Vector[Symbol[_]] =
        method.desc.args
              .map(x => (x.size, makeSymbol(x.prim)))
              .flatMap{case (size, sym) => Seq.fill(size)(sym): Seq[Symbol[_]]}
              .padTo(method.misc.maxLocals, new Symbol(-1, null))
              .toVector

      val (regInsns, stackToSsa, ssaToStack, states) = run(insns, locals, x => makeSymbol(x))

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
            case x: Insn.UnaryBranch[_]   => x.copy(target = stackToSsa(x.target), phi = culled)
            case x: Insn.BinaryBranch[_, _]  => x.copy(target = stackToSsa(x.target), phi = culled)
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

      newInsns.map("| " + _).foreach(println)
      println("converted locals " + symbols.map(_.size).sum)
      newInsns.zipWithIndex
        .splitAll(breaks.toList.sorted)
        .filter(_.length > 0)
        .map(s => (s(0)._2, s.map(_._1)))
        .toMap ->
      symbols.map(_.size).sum
    }
  }
  case class State(stack: List[Symbol[_]], locals: Vector[Symbol[_]])

  def run(insns: Seq[OpCode], locals: Vector[Symbol[_]], makeSymbol: Prim[_] => Symbol[_])(implicit vt: rt.Thread) = {
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

  def op(state: State, op: OpCode, makeSymbol: Prim[_] => Symbol[_])(implicit vt: rt.Thread): (State, Option[Insn]) = op match {
    case InvokeStatic(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize)
      val target = makeSymbol(sig.desc.ret.prim)
      state.copy(stack = target :: newStack) -> Some(Insn.InvokeStatic(target, args, cls, sig))

    case Load(index, _) =>
      state.copy(stack = state.locals(index) :: state.stack) -> None

    case Ldc(thing) =>

      val symbol = thing match{
        case _: Long => makeSymbol(J)
        case _: Double => makeSymbol(D)
        case _ => makeSymbol(I)
      }

      state.copy(stack = symbol :: state.stack) -> Some(Insn.Ldc(symbol.n, thing))

    case BinOp(a, b, out) =>
      val symbol = makeSymbol(out)

      val first :: second :: newStack = state.stack
      state.copy(stack = symbol :: newStack) -> Some(Insn.BinOp(first.cast[Symbol[Any]], second.cast[Symbol[Any]], symbol.cast[Symbol[Any]], op.cast[BinOp[Any, Any, Any]]))

    case UnaryOp(a, prim) =>
      val symbol = makeSymbol(prim)
      val first :: newStack = state.stack
      state.copy(stack = symbol :: newStack) -> Some(Insn.UnaryOp(first.cast[Symbol[Any]], symbol.cast[Symbol[Any]], op.cast[UnaryOp[Any, Any]]))

    case Store(index, Prim(size)) =>
      state.copy(stack = state.stack.tail, locals = state.locals.patch(index, Seq.fill(size)(state.stack.head.cast[Symbol[Any]]), size)) -> None

    case UnaryBranch(index) =>
      state.copy(stack = state.stack.tail) -> Some(Insn.UnaryBranch(state.stack.head, index, op))

    case BinaryBranch(index) =>
      val first :: second :: newStack = state.stack
      state.copy(stack = newStack) -> Some(Insn.BinaryBranch(first, second, index, op))

    case BinaryBranchObj(index) =>
      val first :: second :: newStack = state.stack
      state.copy(stack = newStack) -> Some(Insn.BinaryBranch(first, second, index, op))

    case ReturnVal(n) =>
      state.copy(stack = state.stack.drop(n)) -> Some(Insn.ReturnVal(state.stack.take(n)))

    case Goto(index) =>
      state -> Some(Insn.Goto(index))

    case Push(v) =>
      val symbol = makeSymbol(I)
      state.copy(stack = symbol :: state.stack) -> Some(Insn.Push(symbol.cast[Symbol[Any]].prim, symbol.n, v))

    case o @ Const(prim) =>
      val symbol = makeSymbol(prim)
      state.copy(stack = symbol :: state.stack) -> Some(Insn.Push(symbol.cast[Symbol[Any]].prim, symbol.n, o.value))

    case New(desc) =>
      state.copy(stack = makeSymbol(I) :: state.stack) ->
        Some(Insn.New(vt.vm.ClsTable(desc)))


  }

}