package svm.parsing.opcodes

import svm.{Frame, VmThread, VirtualMachine}
import svm.parsing.ConstantInfo


object OpCodeGen {

  case class Context(thread: VmThread,
                     frame: Frame,
                     rcp: Seq[Any],
                     stack: List[Any],
                     classes: String => svm.Class,
                     nextByte: () => Byte,
                     returnVal: Option[Any] => Unit){

    def twoBytes() = nextByte() << 8 | nextByte()
  }

  object OpCode{ // mutates the world
    def unapply(o: OpCode) = (o.id, o.name, o.op)
    def apply(id: Byte, name: String)(op: Context => Unit = (x) => ()) =
      BaseOpCode(id, name, op)
  }

  case class BaseOpCode(id: Byte, name: String, op: Context => Unit) extends OpCode
  trait OpCode{
    def id: Byte
    def name: String
    def op: Context => Unit
  }


  object StackOpCode{
    def apply(id: Byte, name: String)(stackOp: (Context, List[Any]) => List[Any]) =
      BaseStackOpCode(id, name, stackOp)
  }
  case class BaseStackOpCode(id: Byte, name: String, stackOp: (Context, List[Any]) => List[Any]) extends StackOpCode
  trait StackOpCode extends OpCode{
    def op = (ctx) => ctx.frame.stack = stackOp(ctx, ctx.stack)
    def stackOp: (Context, List[Any]) => List[Any]
  }

  case class PureStackOpCode(id: Byte, name: String)(pureStackOp: List[Any] => List[Any]) extends StackOpCode{
    def stackOp = (_, s) => pureStackOp(s)
  }
  case class PushOpCode(id: Byte, name: String, value: Any) extends StackOpCode{
    def stackOp = (_, stack) => value :: stack
  }

  case class PushValOpCode(id: Byte, name: String, valOp: Context => Any) extends StackOpCode{
    def stackOp = (ctx, stack) => valOp(ctx) :: stack
  }

  case class PushConstOpCode(id: Byte, name: String, pop: Context => Int) extends StackOpCode{
    def stackOp = (ctx, stack) => ctx.rcp(pop(ctx)) :: stack
  }

  case class StoreLocal(id: Byte, name: String, index: Int) extends OpCode{
    def op = { ctx =>
      val (last :: newOpStack) = ctx.frame.stack
      val newIndex = if (index > 0) index else ctx.nextByte()
      ctx.frame.locals(newIndex) = last
      ctx.frame.stack = newOpStack
    }
  }

  case class PushLocalIndexed(id: Byte, name: String, index: Int) extends OpCode{
    def op = { ctx =>
      val newIndex = if (index > 0) index else ctx.nextByte()
      ctx.frame.stack :+ ctx.frame.locals(newIndex)
    }
  }

  case class PushFromArray(id: Byte, name: String) extends StackOpCode{
    def stackOp = { case (_, (index: Int) :: (arrayRef: Seq[Any]) :: baseStack) =>
      arrayRef(index) :: baseStack
    }
  }

  case class StoreArray(id: Byte, name: String) extends StackOpCode{
    def stackOp = { case (_, value :: (index: Int) :: (arrayRef: Seq[Any]) :: baseStack ) =>
      arrayRef.updated(index, value) :: baseStack
    }
  }

  trait Branch extends OpCode{
    def op = { ctx =>
      val (offset, newStack) = branchOp(ctx, ctx.thread, ctx.stack)
      ctx.frame.pc = ctx.frame.pc + offset
      ctx.thread.threadStack.head.stack = newStack
    }

    def branchOp: (Context, VmThread, List[Any]) => (Int, List[Any])
  }
  case class UnaryBranch(id: Byte, name: String)(bop: Int => Boolean) extends Branch{
    def branchOp = { case (ctx, thread, (top: Int) :: baseStack) =>
      (if (bop(top)){ ctx.twoBytes() }else 0 , baseStack)
    }
  }
  case class BinaryBranch(id: Byte, name: String)(bop: (Int, Int) => Boolean) extends Branch{
    def branchOp = { case (ctx, thread, (top: Int) :: (next: Int) :: baseStack) =>
      (if (bop(top, next)){ ctx.twoBytes() }else 0, baseStack)
    }
  }

  case class JsrBranch(id: Byte, name: String) extends Branch{
    def branchOp = { case (ctx, thread, (top: Int) :: (next: Int) :: baseStack) =>
      (ctx.twoBytes(), baseStack)
    }
  }

  case class RetBranch(id: Byte, name: String) extends Branch{
    def branchOp = { case (ctx, thread, (top: Int) :: (second: Int) :: baseStack) =>
      (thread.threadStack.head.locals(ctx.nextByte()).asInstanceOf[Int], baseStack)
    }
  }


}
