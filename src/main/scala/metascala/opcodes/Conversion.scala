package metascala
package opcodes

import metascala.imm.{Attached, Type, Method}
import scala.collection.mutable
import metascala.StackOps.OpCode
import org.objectweb.asm.commons.AdviceAdapter
import imm.Type.Prim
import imm.Type.Prim._
import scala.annotation.tailrec

case class Symbol(n: Int, tpe: imm.Type){
  override def toString = if (n < 0) "~" else ""+n
  def size = tpe.size
  def slots = n.until(n+size)
  def join(l: List[Symbol]) = {
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
  def convertToSsa(method: Method, cls: String)(implicit vm: VM): Code = {
    println(s"-------------------Converting: $cls/${method.sig}--------------------------")

    val thisArg = if(method.static) Nil else Seq(imm.Type.
      Cls("java/lang/Object"))


    val locals: Vector[imm.Type] = method.desc.args
                                         .++:(thisArg)
                                         .toVector

    var insns = method.code.insns.toList
    val restAttached = method.code.attachments.toList
    var attached = (restAttached.head :+ imm.Attached.Frame(Nil, locals)) +: restAttached.tail
    var state: State = null

    val blockMap = new Array[Int](insns.length)
    val blocks = mutable.Buffer.empty[(State, Seq[Insn], State)]
    var maxSyms = 0
    while (insns != Nil){
      var symCount = 0
      def makeSymbol(t: Type) = {
        val sym = Symbol(symCount, t)
        symCount += t.size
        sym
      }
      state =
        attached.head
          .collectFirst{case f: imm.Attached.Frame => f}
          .map(State(_, makeSymbol _))
          .getOrElse(state)
      val (regInsns, newInsns, newAttached, newState) = run(insns, attached, state, makeSymbol _)
      val index = method.code.insns.length - insns.length
      blockMap(index) = blocks.length
      blocks.append((state, regInsns, newState))
      maxSyms = maxSyms max symCount
      insns = newInsns
      attached = newAttached
      state = newState
    }
    println("Block Map " + blockMap.toList)
    val rewiredBlocks = blocks.map{ case (before, buffer, after) => (before, buffer.map{
      case x: Insn.UnaryBranch  => x.copy(target = blockMap(x.target))
      case x: Insn.BinaryBranch => x.copy(target = blockMap(x.target))
      case x: Insn.Goto         => x.copy(target = blockMap(x.target))
      case x => x
    }, after)}

    for((x, i) <- rewiredBlocks.zipWithIndex){
      println()
      println(i + "\t" + x._1)
      x._2.foreach(println)
    }
    println("============================================")

    val phis: Seq[Seq[Seq[(Sym, Sym)]]] =
      for{((destState, buffer, _), i) <- rewiredBlocks.zipWithIndex} yield {
        for{((_, srcBuffer, srcState), j) <- rewiredBlocks.zipWithIndex} yield {
          if (!srcBuffer.last.targets.contains(i) && j + 1 != i) Nil else {
            val zipped = (srcState.locals zip destState.locals) ++
                     (srcState.stack.reverse zip destState.stack.reverse)
            for {
              (src, dest)<- zipped.distinct
              if src != dest
              pairs <- src.slots zip dest.slots
            } yield pairs
          }
        }
      }
    println("PHIS")
    phis.foreach(println)
    //case class Code(blocks: Seq[BasicBlock] = Nil, localSize: Int = 0)
    //case class BasicBlock(insns: Seq[Insn], phi: Seq[Seq[(Sym, Sym)]])
    val basicBlocks =
      rewiredBlocks.map(_._2)
            .zip(phis)
            .map(BasicBlock.tupled)
    println("----------------------------------------------")
    for ((block, i) <- basicBlocks.zipWithIndex){
      println()
      println(i + "\t" + block.phi.toList)
      block.insns.foreach(println)
    }
    println("----------------------------------------------")
    Code(basicBlocks, maxSyms)
  }
  @tailrec
  def run(insns: List[OpCode],
          attached: List[Seq[Attached]],
          state: State,
          makeSymbol: Type => Symbol,
          regInsns: Seq[Insn] = Seq())
         (implicit vm: VM): (Seq[Insn], List[OpCode], List[Seq[Attached]], State) = {

    (insns, attached) match {
      case (i :: is, a :: as) =>

        println(i + "\t" + i.toString.padTo(30, ' ') + state.stack.toString.padTo(20, ' ') + state.locals.toString.padTo(30, ' '))
        val (newState, newInsn) = op(state, i, makeSymbol)
        val outInsns = regInsns ++ newInsn
        import StackOps._
        (i, as) match{
          case (_, a :: _) if a.exists(_.isInstanceOf[Attached.Frame]) =>
            println("A")
            (outInsns, is, as, newState)
          case (_: Jump, _) =>
            println("B")
            (outInsns, is, as, newState)
          case _ => run(is, as, newState, makeSymbol, outInsns)
        }
      case _ =>
        println("C")
        (regInsns, insns, attached, state)
    }
  }


  case class State(stack: List[Symbol], locals: Vector[Symbol]) extends imm.Attached
  object State{
    def apply(f: imm.Attached.Frame, makeSymbol: Type => Symbol): State = {
      State(
        f.stack.map(makeSymbol).flatMap(s => Seq.fill(s.size)(s)),
        f.locals.map(makeSymbol).flatMap(s => Seq.fill(s.size)(s))
      )
    }
  }

  import StackOps._
  def op(state: State, oc: OpCode, makeSymbol: Type => Symbol)(implicit vm: VM): (State, Seq[Insn]) = oc match {

    case InvokeStatic(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize)
      val target = makeSymbol(sig.desc.ret)
      state.copy(stack = target join newStack) -> List(Insn.InvokeStatic(target.n, args.reverse.map(_.n), cls, sig))

    case InvokeSpecial(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val target = makeSymbol(sig.desc.ret)
      state.copy(stack = target join newStack) -> List(Insn.InvokeSpecial(target.n, args.reverse.map(_.n), cls, sig))

    case InvokeVirtual(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val target = makeSymbol(sig.desc.ret)
      state.copy(stack = target join newStack) -> List(Insn.InvokeVirtual(target.n, args.reverse.map(_.n), cls.cast[imm.Type.Cls], sig))

    case NewArray(typeCode) =>
      val length :: rest = state.stack
      val symbol = makeSymbol(I)
      val typeRef: imm.Type = typeCode match{
        case 4  => Z: imm.Type
        case 5  => C: imm.Type
        case 6  => F: imm.Type
        case 7  => D: imm.Type
        case 8  => B: imm.Type
        case 9  => S: imm.Type
        case 10 => I: imm.Type
        case 11 => J: imm.Type
      }
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

    case BinOp(a, b, out, func) =>
      val symbol = makeSymbol(out)
      val (symA, stack1) = state.stack.pop(a)
      val (symB, stack2) = stack1.pop(b)

      state.copy(stack = symbol join stack2) -> List(Insn.BinOp(symA.n, a, symB.n, b, symbol.n, out, func))

    case UnaryOp(a, prim, func) =>
      val symbol = makeSymbol(prim)
      val (symA, stack1) = state.stack.pop(a)
      state.copy(stack = symbol join stack1) -> List(Insn.UnaryOp(symA.n, a, symbol.n, prim, func))

    case Store(index, p) =>
      val (popped, rest) = state.stack.pop(p)
      state.copy(stack = rest, locals = state.locals.padTo(index, Symbol(-1, V)).patch(index, Seq.fill(p.size)(popped), p.size)) -> Nil

    case UnaryBranch(index, func) =>
      val head :: tail = state.stack
      state.copy(stack = tail) -> List(Insn.UnaryBranch(head.n, index, func))

    case BinaryBranch(index, func) =>
      val first :: second :: newStack = state.stack
      state.copy(stack = newStack) -> List(Insn.BinaryBranch(first.n, second.n, index, func))

    case ReturnVal(n) =>
      val returned = if (n == 0) new Symbol(0, V) else state.stack.head
      state.copy(stack = state.stack.drop(n)) -> List(Insn.ReturnVal(returned.n))

    case Goto(index) =>
      state -> List(Insn.Goto(index))

    case Push(v) =>
      val symbol = makeSymbol(I)
      state.copy(stack = symbol join state.stack) -> List(Insn.Push(I, symbol.n, v))

    case o @ Const(prim, value) =>
      val symbol = makeSymbol(prim)
      state.copy(stack = symbol join state.stack) -> List(Insn.Push(prim, symbol.n, o.value))

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

      val symbol = makeSymbol(I)
      val out = makeSymbol(I)
      state.copy(locals = state.locals.updated(varId, out)) -> Seq(
        Insn.Ldc(symbol.n, amount),
        Insn.BinOp[I, I, I](symbol.n, I, state.locals(varId).n, I, out.n, I, _+_)
      )

    case ManipStack(transform) =>
      state.copy(stack = transform(state.stack).cast[List[Symbol]]) -> Nil

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
          Insn.BinaryBranch(state.stack.head.n, symbol.n, t, _ == _)
        )
      }.:+(Insn.Goto(default))
    case TableSwitch(min, max, default, targets) =>
      state.copy(stack = state.stack.tail) -> min.to(max).zip(targets).flatMap{ case (k, t) =>
        val symbol = makeSymbol(I)
        Seq(
          Insn.Ldc(symbol.n, k),
          Insn.BinaryBranch(state.stack.head.n, symbol.n, t, _ == _)
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