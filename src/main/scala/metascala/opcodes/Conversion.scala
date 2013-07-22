package metascala
package opcodes

import metascala.imm.{TryCatchBlock, Attached, Type, Method}
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
  type Blocks = Seq[(State, Seq[Insn], State, Seq[Type])]

  def convertToSsa(method: Method, cls: String)(implicit vm: VM): Code = {
    println(s"-------------------Converting: $cls/${method.sig}--------------------------")
    if (method.name == "copy") {
      method.code.insns.zipWithIndex.foreach{ case (x, i) =>
        println(s"$i\t$x")
      }
      println("---TryCatch---")
      method.misc.tryCatchBlocks.foreach{ b =>
        println(b.start + " - " + b.end + ":\t" + b.handler)
      }
    }

    val (blocks, tryCatchBlocks) = walkBlocks(method)

    val basicBlocks =
      blocks.map(b => b._2 -> b._4)
            .zip(makePhis(blocks))
            .map{ case ((buff, types), phis) => BasicBlock(buff, phis, types) }

    if(method.name == "copy"){
      println("----------------------------------------------")
      for ((block, i) <- basicBlocks.zipWithIndex){
        println()
        println(i + "\t" + block.phi.toList)
        block.insns.foreach(println)
      }
      println("---TryCatch---")
      tryCatchBlocks.foreach(println)
      println("----------------------------------------------")
    }

    Code(basicBlocks, tryCatchBlocks)
  }

  /**
   * Calculates the phi node movements required at the top of each basic block
   * to complete the SSA code.
   */
  def makePhis(blocks: Blocks): Seq[Seq[Seq[(Sym, Sym)]]] =
    for{((destState, buffer, _, _), i) <- blocks.zipWithIndex} yield {
      for{((_, srcBuffer, srcState, _), j) <- blocks.zipWithIndex} yield {
        def stuff = {
          val zipped = (srcState.locals zip destState.locals) ++
            (srcState.stack.reverse zip destState.stack.reverse)
          for {
            (src, dest)<- zipped.distinct
            if src != dest
            pairs <- src.slots zip dest.slots
          } yield pairs
        }
        if (j + 1 == i) stuff
        else if(!srcBuffer.isEmpty && srcBuffer.last.targets.contains(i)) stuff
        else Nil
      }
    }

  /**
   * Takes a method and breaks up the bytecode from a flat list of stack
   * operations into a list of basic blocks of register operations
   */
  def walkBlocks(method: imm.Method)(implicit vm: VM): (Blocks, Seq[opcodes.TryCatchBlock]) = {


    val locals: Vector[imm.Type] ={
      val thisArg =
        if(method.static) Nil
        else Seq(imm.Type.Cls("java/lang/Object"))

      method.desc
        .args
        .++:(thisArg)
        .toVector
    }

    var insns = method.code.insns.toList

    var attached = {
      val restAttached = method.code.attachments.toList
      (restAttached.head :+ imm.Attached.Frame(Nil, locals)) +: restAttached.tail
    }

    var state: State = null
    val allStates = mutable.Buffer.empty[State]

    val blockMap = new Array[Int](insns.length)
    val blocks = mutable.Buffer.empty[(State, Seq[Insn], State, Seq[Type])]
    var sections: Seq[Seq[Int]] =  Nil

    object makeSymbol{
      var symCount = 0
      val types = mutable.Buffer.empty[Type]
      def apply(t: Type) = {
        types.append(t)
        val sym = Symbol(symCount, t)
        symCount += t.size
        sym
      }
    }

    while (insns != Nil){
      allStates.append(
        attached.head
                .collectFirst{case f: imm.Attached.Frame => f}
                .map(State(_, makeSymbol.apply _))
                .getOrElse(state)
      )

      val (regInsns, newInsns, newAttached, newState) = run(insns, attached, allStates.last, makeSymbol.apply)
      sections = sections :+ regInsns.map(_.length).scan(0)(_+_)

      blockMap(method.code.insns.length - insns.length) = blocks.length

      blocks.append((allStates.last, regInsns.flatten, newState, makeSymbol.types))
      insns = newInsns
      attached = newAttached
      state = newState
    }


    var current = 0
    for (i <- 0 until blockMap.length){
      current = current max blockMap(i)
      blockMap(i) = current
    }

    val tryCatchBlocks = method.misc.tryCatchBlocks.map{b =>
      val (startBlock, endBlock) = (blockMap(b.start), blockMap(b.end))
      opcodes.TryCatchBlock(
        startBlock -> sections(startBlock)(b.start - blockMap.indexOf(startBlock)),
        endBlock -> sections(endBlock)(b.end - blockMap.indexOf(endBlock)),
        blockMap(b.handler),
        allStates(blockMap(b.handler)).stack(0).n,
        b.blockType
      )
    }
//    println("Block Map " + blockMap.toList)
    val finalBlocks = blocks.map{ case (before, buffer, after, types) =>
      val newBuffer = buffer.map{
        case x: Insn.UnaryBranch  => x.copy(target = blockMap(x.target))
        case x: Insn.BinaryBranch => x.copy(target = blockMap(x.target))
        case x: Insn.Goto         => x.copy(target = blockMap(x.target))
        case x: Insn.LookupSwitch => x.copy(targetList = x.targetList.map(blockMap), default = blockMap(x.default))
        case x: Insn.TableSwitch  => x.copy(targetList = x.targetList.map(blockMap), default = blockMap(x.default))
        case x => x
      }
      (before, newBuffer, after, types)
    }
    (finalBlocks, tryCatchBlocks)
  }
  @tailrec
  def run(insns: List[OpCode],
          attached: List[Seq[Attached]],
          state: State,
          makeSymbol: Type => Symbol,
          regInsns: Seq[Seq[Insn]] = Seq(),
          index: Int = 0)
         (implicit vm: VM): (Seq[Seq[Insn]], List[OpCode], List[Seq[Attached]], State) = {

    (insns, attached) match {
      case (i :: is, a :: as) =>

//        println(i + "\t" + i.toString.padTo(30, ' ') + state.stack.toString.padTo(20, ' ') + state.locals.toString.padTo(30, ' '))
        val (newState, newInsn) = op(state, i, makeSymbol)
        val outInsns = regInsns ++ Seq(newInsn)
        import StackOps._
        (i, as) match{
          case (_, a :: _) if a.exists(_.isInstanceOf[Attached.Frame]) => (outInsns, is, as, newState)
          case (_: Jump, _) => (outInsns, is, as, newState)
          case _ => run(is, as, newState, makeSymbol, outInsns, index + 1)
        }
      case _ => (regInsns, insns, attached, state)
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

  implicit class poppable[T](val l: List[T]){
    def pop(p: Prim[_]) = {
      val t = l.splitAt(p.size)
      t.copy(_1 = t._1.head)
    }
  }
  /**
   * Performs a single step in the symbolic interpretation of a method; does
   * everything pure-functional style, taking the old state in and spitting
   * the new state out the other end, togther with any register instructions
   * that are needed.
   */
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

    case InvokeInterface(cls, sig) =>
      val (args, newStack) = state.stack.splitAt(sig.desc.argSize + 1)
      val target = makeSymbol(sig.desc.ret)
      state.copy(stack = target join newStack) -> List(Insn.InvokeInterface(target.n, args.reverse.map(_.n), cls.cast[imm.Type.Cls], sig))

    case NewArray(typeCode) =>
      val length :: rest = state.stack
      val symbol = makeSymbol(A)
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
      val symbol = makeSymbol(A)
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
        case _ => makeSymbol(A)
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

    case Const(prim, value) =>
      val symbol = makeSymbol(prim)
      state.copy(stack = symbol join state.stack) -> List(Insn.Push(prim, symbol.n, value))

    case New(desc) =>
      val symbol = makeSymbol(A)
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
      state.copy(stack = state.stack.tail) -> Seq(
        Insn.LookupSwitch(state.stack.head.n, default, keys, targets)
      )
    case TableSwitch(min, max, default, targets) =>
      state.copy(stack = state.stack.tail) -> Seq(
        Insn.TableSwitch(state.stack.head.n, min, max, default, targets)
      )

    case InstanceOf(desc) =>
      val head :: rest = state.stack
      val symbol = makeSymbol(I)
      state.copy(stack = symbol :: rest) -> Seq(Insn.InstanceOf(head.n, symbol.n, desc))

    case MultiANewArray(desc, dims) =>
      val (dimsX, rest) = state.stack.splitAt(dims)
      val symbol = makeSymbol(A)
      state.copy(stack = symbol :: rest) -> Seq(Insn.MultiANewArray(desc, symbol.n, dimsX.map(_.n)))
  }
}