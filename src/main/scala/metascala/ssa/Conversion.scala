package metascala
package ssa

import metascala.imm.{Type, Method}
import scala.collection.mutable
import metascala.opcodes.OpCode

import metascala.Prim

import metascala.ssa.Symbol
import opcodes._
case class Symbol[T](n: Int, prim: Prim[T]){
  override def toString = ""+n
  def size = prim.size
  def slots = n.until(n+prim.size)
  def join(l: List[Symbol[_]]) = {
    List.fill(size)(this) ::: l
  }
}

object Conversion {
  implicit class poppable[T](val l: List[T]){
    def pop(p: Prim[_]) = {
      val t = l.splitAt(p.size)
      t.copy(_1 = t._1.head)
    }
  }
  def convertToSsa(method: Method, cls: String)(implicit vm: VM): (Map[Int, Seq[Insn]], Int) = {
    println(s"-------------------Converting: $cls/${method.sig}--------------------------")
    val insns = method.code.insns
    if (insns.isEmpty) {
      Map() -> 0
    } else {
      val symbols = mutable.Buffer[Symbol[_]]()
      def makeSymbol[T](prim: Prim[T]): Symbol[T] = {
        val newSym = new Symbol(symbols.length, prim)
        for (i <- 0 until prim.size) symbols.append(newSym)

        newSym
      }

      val thisArg = if(method.static) Nil else Seq(imm.Type.Cls("java/lang/Object"))
      val locals: Vector[Symbol[_]] =
        method.desc.args
              .++:(thisArg)
              .map(x => (x.size, makeSymbol(x.prim)))
              .flatMap{case (size, sym) => Seq.fill(size)(sym): Seq[Symbol[_]]}
              .padTo(method.misc.maxLocals, new Symbol(-1, null))
              .toVector

      val (regInsns, stackToSsa, ssaToStack, states) = run(insns, locals, x => makeSymbol(x))
      println(ssaToStack)
      println(states.length)
      val newInsns = regInsns.zipWithIndex.map{
        case (x: Jump, i) =>
          val postState = states(ssaToStack(i + 1) + 1)
          val targetState = states(x.target)
          val fullZipped =
            postState.locals.++(postState.stack)
                     .zip(targetState.locals.++(targetState.stack))

          val culled =
            fullZipped.distinct
              .filter{case (a, b) => (a.n != -1) && (b.n != -1) && (a.n != b.n)}
              .toList
              .flatMap{case (a, b) =>
              a.slots.zip(b.slots)
            }

          x match{
            case x: Insn.UnaryBranch[_]     => x.copy(target = stackToSsa(x.target), phi = culled)
            case x: Insn.BinaryBranch[_, _] => x.copy(target = stackToSsa(x.target), phi = culled)
            case x: Insn.Goto               => x.copy(target = stackToSsa(x.target), phi = culled)
            case x => x
          }
        case (x, i) => x
      }

      val breaks =
        newInsns.toSeq
          .zipWithIndex
          .flatMap{
            case (x: Jump, i) => Seq(x.target, i);
            case _ => Nil
          }
          .distinct
          .sorted

      newInsns.zipWithIndex
              .map{case (x, i) => "| " + i + "\t" + x}
              .foreach(println)
      println(s"-------------------Completed: ${method.sig}--------------------------")
      println(symbols)
      println(symbols.map(_.size))
      newInsns.zipWithIndex
        .splitAll(breaks.toList.sorted)
        .filter(_.length > 0)
        .map(s => (s(0)._2, s.map(_._1)))
        .toMap ->
      symbols.map(_.size).sum
    }
  }
  case class State(stack: List[Symbol[_]], locals: Vector[Symbol[_]])

  def run(insns: Seq[OpCode], locals: Vector[Symbol[_]], makeSymbol: Prim[_] => Symbol[_])(implicit vm: VM) = {
    val regInsns = mutable.Buffer[Insn]()
    val stackToSsa = mutable.Buffer[Int]()
    val states = mutable.Buffer[State](State(Nil, locals))
    for ((insn, i) <- insns.zipWithIndex){
      stackToSsa += regInsns.length
      println(i + "\t" + insn.toString.padTo(30, ' ') + states.last.stack.toString.padTo(20, ' ') + states.last.locals.toString.padTo(30, ' '))
      val (newState, newInsn) = op(states.last, insn, makeSymbol)

      states.append(newState)
      newInsn.map{ regInsns.append(_) }
    }

    val ssaToStack =
      0.to(stackToSsa.max)
        .map(stackToSsa.drop(1).indexOf(_))
        .:+(stackToSsa.length-1)

    (regInsns, stackToSsa, ssaToStack, states)
  }




  def op(state: State, oc: OpCode, makeSymbol: Prim[_] => Symbol[_])(implicit vm: VM): (State, Seq[Insn]) = oc match {

    case InvokeStatic(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize)
      val target = makeSymbol(sig.desc.ret.prim)
      state.copy(stack = target join newStack) -> List(Insn.InvokeStatic(target.n, args.reverse.map(_.n), cls, sig))

    case InvokeSpecial(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val target = makeSymbol(sig.desc.ret.prim)
      state.copy(stack = target join newStack) -> List(Insn.InvokeSpecial(target.n, args.reverse.map(_.n), cls, sig))

    case InvokeVirtual(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val target = makeSymbol(sig.desc.ret.prim)
      state.copy(stack = target join newStack) -> List(Insn.InvokeVirtual(target.n, args.reverse.map(_.n), cls.cast[imm.Type.Cls], sig))

    case NewArray(typeCode) =>
      val length :: rest = state.stack
      val symbol = makeSymbol(I)
      val typeRef = imm.Type.Prim(
        typeCode match{
          case 4  => 'Z'
          case 5  => 'C'
          case 6  => 'F'
          case 7  => 'D'
          case 8  => 'B'
          case 9  => 'S'
          case 10 => 'I'
          case 11 => 'J'
        }
      )
      state.copy(stack = symbol :: rest) -> List(Insn.NewArray(length.n, symbol.n, typeRef))
    case MonitorEnter | MonitorExit =>
      val monitor :: rest = state.stack
      state.copy(stack = rest) -> Nil
    case ArrayLength =>
      val arr :: rest = state.stack
      val symbol = makeSymbol(I)
      state.copy(stack = symbol :: rest) -> List(Insn.ArrayLength(arr.n, symbol.n))
    case ANewArray(typeRef) =>
      val length :: rest = state.stack
      val symbol = makeSymbol(I)
      state.copy(stack = symbol :: rest) -> List(Insn.NewArray(length.n, symbol.n, typeRef))

    case LoadArray(prim) =>
      val index :: array :: base = state.stack
      val symbol = makeSymbol(prim)

      state.copy(stack = symbol join base) -> List(Insn.LoadArray(symbol.n, index.n, array.n, prim))

    case StoreArray(prim) =>
      val (value, index :: array :: base) = state.stack.pop(prim)
      state.copy(stack = base) -> List(Insn.StoreArray(value.n, index.n, array.n, prim))

    case Load(index, _) =>
      state.copy(stack = state.locals(index) join state.stack) -> Nil

    case Ldc(thing) =>
      val symbol = thing match{
        case _: Long => makeSymbol(J)
        case _: Double => makeSymbol(D)
        case _ => makeSymbol(I)
      }

      state.copy(stack = symbol join state.stack) -> List(Insn.Ldc(symbol.n, thing))

    case BinOp(a, b, out) =>
      val symbol = makeSymbol(out)
      val (symA, stack1) = state.stack.pop(a)
      val (symB, stack2) = stack1.pop(b)

      state.copy(stack = symbol join stack2) -> List(Insn.BinOp(symA.n, symB.n, symbol.n, oc.cast[BinOp[Any, Any, Any]]))

    case UnaryOp(a, prim) =>
      val symbol = makeSymbol(prim)
      val (symA, stack1) = state.stack.pop(a)
      state.copy(stack = symbol join stack1) -> List(Insn.UnaryOp(symA.n, symbol.n, oc.cast[UnaryOp[Any, Any]]))

    case Store(index, Prim(size)) =>
      state.copy(stack = state.stack.tail, locals = state.locals.patch(index, Seq.fill(size)(state.stack.head.cast[Symbol[Any]]), size)) -> Nil

    case UnaryBranch(index) =>
      val head :: tail = state.stack
      state.copy(stack = tail) -> List(Insn.UnaryBranch(head.n, index, oc.cast[UnaryBranch]))

    case BinaryBranch(index) =>
      val first :: second :: newStack = state.stack
      state.copy(stack = newStack) -> List(Insn.BinaryBranch(first.n, second.n, index, oc.cast[BinaryBranch]))

    case ReturnVal(n) =>
      val returned = if (n == 0) new Symbol(0, V) else state.stack.head
      state.copy(stack = state.stack.drop(n)) -> List(Insn.ReturnVal(returned.n))

    case Goto(index) =>
      state -> List(Insn.Goto(index))

    case Push(v) =>
      val symbol = makeSymbol(I)
      state.copy(stack = symbol join state.stack) -> List(Insn.Push(symbol.cast[Symbol[Any]].prim, symbol.n, v))

    case o @ Const(prim) =>
      val symbol = makeSymbol(prim)
      state.copy(stack = symbol join state.stack) -> List(Insn.Push(symbol.cast[Symbol[Any]].prim, symbol.n, o.value))

    case New(desc) =>
      val symbol = makeSymbol(I)
      state.copy(stack = symbol join state.stack) ->
        List(Insn.New(symbol.n, vm.ClsTable(desc)))

    case PutStatic(owner, name, tpe) =>
      val index = owner.cls.staticList.indexWhere(_.name == name)
      val prim = owner.cls.staticList(index).desc.prim
      val (symbol, rest) = state.stack.pop(prim)
      state.copy(stack = rest) ->
      List(Insn.PutStatic(symbol.n, owner.cls, index, prim))

    case GetStatic(owner, name, tpe) =>
      val index = owner.cls.staticList.indexWhere(_.name == name)
      val prim = owner.cls.staticList(index).desc.prim
      val symbol = makeSymbol(prim)
      state.copy(stack = symbol join state.stack) ->
        List(Insn.GetStatic(symbol.n, owner.cls, index, prim))

    case PutField(owner, name, tpe) =>
      val index = owner.cls.fieldList.lastIndexWhere(_.name == name)
      val prim = owner.cls.fieldList(index).desc.prim

      val (symA, stack1) = state.stack.pop(tpe.prim)
      val (symB, stack2) = stack1.pop(I)

      state.copy(stack = stack2) ->
        List(Insn.PutField(symA.n, symB.n, index, prim))

    case GetField(owner, name, tpe) =>
      val index = owner.cls.fieldList.lastIndexWhere(_.name == name)
      val prim = owner.cls.fieldList(index).desc.prim
      val symbol = makeSymbol(prim)
      val (sym, stack1) = state.stack.pop(tpe.prim)

      state.copy(stack = symbol join stack1) ->
        List(Insn.GetField(symbol.n, sym.n, index, prim))

    case IInc(varId, amount) =>

      val symbol = makeSymbol(I).cast[Symbol[Int]]
      val out = makeSymbol(I).cast[Symbol[Int]]
      state.copy(locals = state.locals.updated(varId, out)) -> Seq(
        Insn.Ldc(symbol.n, amount),
        Insn.BinOp(symbol.n, state.locals(varId).n, out.n, IAdd.cast[BinOp[Int, Int, Int]])
      )

    case ManipStack(transform) =>
      state.copy(stack = transform(state.stack).cast[List[Symbol[_]]]) -> Nil

    case AThrow =>
      val ex :: rest = state.stack
      state.copy(stack = rest) -> Seq(Insn.AThrow(ex.n))

    case CheckCast(desc) =>
      state -> Seq(Insn.CheckCast(state.stack.head.n, desc))

    case LookupSwitch(default: Int, keys: Seq[Int], targets: Seq[Int]) =>
      state.copy(stack = state.stack.tail) -> keys.zip(targets).flatMap{ case (k, t) =>
        val symbol = makeSymbol(I)
        Seq(
          Insn.Ldc(symbol.n, k),
          Insn.BinaryBranch(state.stack.head.n, symbol.n, t, IfICmpEq(t))
        )
      }.:+(Insn.Goto(default))
    case TableSwitch(min, max, default, targets) =>
      state.copy(stack = state.stack.tail) -> min.to(max).zip(targets).flatMap{ case (k, t) =>
        val symbol = makeSymbol(I)
        Seq(
          Insn.Ldc(symbol.n, k),
          Insn.BinaryBranch(state.stack.head.n, symbol.n, t, IfICmpEq(t))
        )
      }.:+(Insn.Goto(default))

    case InstanceOf(desc) =>
      val head :: rest = state.stack
      val symbol = makeSymbol(I)
      state.copy(stack = symbol :: rest) -> Seq(Insn.InstanceOf(head.n, symbol.n, desc))

    case MultiANewArray(desc, dims) =>
      val (dimsX, rest) = state.stack.splitAt(dims)
      val symbol = makeSymbol(I)
      state.copy(stack = symbol :: rest) -> Seq(Insn.MultiANewArray(desc, symbol.n, dimsX.map(_.n)))
  }

}