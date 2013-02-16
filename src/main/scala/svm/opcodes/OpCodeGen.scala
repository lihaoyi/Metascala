package svm.model.opcodes

import svm.{Frame, VmThread, VirtualMachine}
import svm.model.ConstantInfo


object OpCodeGen {

  case class Context(vm: VirtualMachine,
                     thread: VmThread,
                     frame: Frame,
                     rcp: Seq[ConstantInfo],
                     stack: List[Any]){
    object Cp{
      def unapply(index: Short): Option[ConstantInfo] = {
        rcp.lift(index)
      }
      def unapply(index: Int): Option[ConstantInfo] = {
        rcp.lift(index)
      }
    }
  }

  object OpCode{ // mutates the world
    def unapply(o: OpCode) = (o.id, o.name, o.op)
    def apply(id: Byte, name: String)(op: (Seq[Byte], Context) => Unit) =
      new OpCode {
        def id = id
        def name = name
        def op = op
      }
  }

  trait OpCode{
    def id: Byte
    def name: String
    def op: (Seq[Byte], Context) => Unit
  }


  object StackOpCode{
    def apply(id: Byte, name: String)(stackOp: (Seq[Byte], Context, List[Any]) => List[Any]) = new StackOpCode {
      def id = id
      def name = name
      def stackOp = stackOp
    }
  }
  trait StackOpCode extends OpCode{
    def op = (bytes, ctx) => ctx.frame.frameStack = stackOp(bytes, ctx, ctx.stack)
    def stackOp: (Seq[Byte], Context, List[Any]) => List[Any]
  }

  case class PureStackOpCode(id: Byte, name: String)(pureStackOp: List[Any] => List[Any]) extends StackOpCode{
    def stackOp = (_, _, s) => pureStackOp(s)
  }
  case class PushOpCode(id: Byte, name: String, value: Any) extends StackOpCode{
    def stackOp = (_, _, stack) => value :: stack
  }

  case class PushValOpCode(id: Byte, name: String, valOp: Seq[Byte] => Any) extends StackOpCode{
    def stackOp = (bytes, _, stack) => valOp(bytes) :: stack
  }

  case class PushConstOpCode(id: Byte, name: String, pop: Seq[Byte] => Int) extends StackOpCode{
    def stackOp = (bytes, _, stack) => pop(bytes) :: stack
  }

  case class StoreLocal(id: Byte, name: String, index: Int) extends OpCode{
    def op = { (inst, ctx) =>
      val (last :: newOpStack) = ctx.frame.frameStack
      val newIndex = if (index > 0) index else inst(1)
      ctx.frame.locals = ctx.frame.locals.updated(newIndex, last)
      ctx.frame.frameStack = newOpStack
    }
  }

  case class PushLocalIndexed(id: Byte, name: String, index: Int) extends OpCode{
    def op = { (inst, ctx) =>
      val newIndex = if (index > 0) index else inst(1)
      ctx.frame.frameStack :+ ctx.frame.locals(newIndex)
    }
  }

  case class PushFromArray(id: Byte, name: String) extends StackOpCode{
    def stackOp = { case (_, _, (index: Int) :: (arrayRef: Seq[Any]) :: baseStack) =>
      arrayRef(index) :: baseStack
    }
  }

  case class StoreArray(id: Byte, name: String) extends StackOpCode{
    def stackOp = { case (_, _, value :: (index: Int) :: (arrayRef: Seq[Any]) :: baseStack ) =>
      arrayRef.updated(index, value) :: baseStack
    }
  }

  trait Branch extends OpCode{
    def op = { (inst, ctx) =>
      val (offset, newStack) = branchOp(inst, ctx.thread, ctx.stack)
      ctx.thread.pc = ctx.thread.pc + offset
      ctx.thread.stack.head.frameStack = newStack
    }

    def branchOp: (Seq[Byte], VmThread, List[Any]) => (Int, List[Any])
  }
  case class UnaryBranch(id: Byte, name: String)(bop: Int => Boolean) extends Branch{
    def branchOp = { case (TwoBytes(i), thread, (top: Int) :: baseStack) =>
      (if (bop(top)){ i }else 0 , baseStack)
    }
  }
  case class BinaryBranch(id: Byte, name: String)(bop: (Int, Int) => Boolean) extends Branch{
    def branchOp = { case (TwoBytes(i), thread, (top: Int) :: (next: Int) :: baseStack) =>
      (if (bop(top, next)){ i }else 0, baseStack)
    }
  }

  case class JsrBranch(id: Byte, name: String) extends Branch{
    def branchOp = { case (TwoBytes(i), thread, (top: Int) :: (next: Int) :: baseStack) =>
      (i, baseStack)
    }
  }

  case class RetBranch(id: Byte, name: String) extends Branch{
    def branchOp = { case (Seq(index), thread, (top: Int) :: (second: Int) :: baseStack) =>
      (thread.stack.head.locals(index).asInstanceOf[Int], baseStack)
    }
  }

  case class ReturnStuff(id: Byte, name: String) extends OpCode{
    def op = { (inst, ctx) =>
      val head :: newFrames = ctx.thread.stack
      ctx.thread.stack = newFrames
      newFrames.head.frameStack = head.frameStack.head :: newFrames.head.frameStack
    }
  }
}
