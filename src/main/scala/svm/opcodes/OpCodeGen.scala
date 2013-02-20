package svm.model.opcodes

import svm.{Frame, VmThread, VirtualMachine}
import svm.model.ConstantInfo
import svm.model.Attribute.Code


object OpCodeGen {

  case class Context(thread: VmThread){
    def classes = thread.classes
    def frame = thread.threadStack.head
    def rcp = frame.runningClass.classFile.constant_pool
    def stack = frame.stack
    def twoBytes() = nextByte() << 8 | nextByte()
    def nextByte() = {
      val bytes = frame.method.attributes.collect{case x: Code => x: Code}.head.bytecodes
      val result = bytes(frame.pc)
      frame.pc += 1
      result
    }
    def returnVal(x: Option[Any]) = {
      thread.threadStack.pop()
      x.foreach(value => thread.threadStack.head.stack = value :: thread.threadStack.head.stack)
    }
  }

  object OpCode{ // mutates the world
    def unapply(o: OpCode) = (o.id, o.name, o.op)
    def apply(id: Byte, name: String, width: Int)(op: Context => Unit = (x) => ()) =
      BaseOpCode(id, name, width, op)
  }

  case class BaseOpCode(id: Byte, name: String, width: Int, op: Context => Unit) extends OpCode
  trait OpCode{
    def id: Byte
    def name: String
    def width: Int
    def op: Context => Unit
  }


  object StackOpCode{
    def apply(id: Byte, name: String, width: Int)(stackOp: (Context, List[Any]) => List[Any]) =
      BaseStackOpCode(id, name, width, stackOp)
  }
  case class BaseStackOpCode(id: Byte, name: String, width: Int, stackOp: (Context, List[Any]) => List[Any]) extends StackOpCode
  trait StackOpCode extends OpCode{
    def op = (ctx) => ctx.frame.stack = stackOp(ctx, ctx.stack)
    def stackOp: (Context, List[Any]) => List[Any]
  }

  case class PureStackOpCode(id: Byte, name: String)(pureStackOp: List[Any] => List[Any]) extends StackOpCode{
    def width = 0
    def stackOp = (_, s) => pureStackOp(s)
  }

  case class PushOpCode[T](id: Byte, name: String, value: T) extends StackOpCode{
    def width = 0
    def stackOp = (_, stack) => value :: stack
  }

  case class PushValOpCode(id: Byte, name: String, width: Int, valOp: Context => Any) extends StackOpCode{
    def stackOp = (ctx, stack) => valOp(ctx) :: stack
  }

  case class PushConstOpCode(id: Byte, name: String, width: Int, pop: Context => Int) extends StackOpCode{
    def stackOp = (ctx, stack) => ctx.rcp(pop(ctx)) :: stack
  }

  case class StoreLocal(id: Byte, name: String, index: Int) extends OpCode{
    def width = if (index >= 0) 0 else 1
    def op = { ctx =>
      val (last :: newOpStack) = ctx.frame.stack
      val newIndex = if (index >= 0) index else ctx.nextByte()
      ctx.frame.locals(newIndex) = last
      ctx.frame.stack = newOpStack
    }
  }

  case class PushLocalIndexed(id: Byte, name: String, index: Int) extends OpCode{
    def width = if (index >= 0) 0 else 1
    def op = { ctx =>
      val newIndex = if (index >= 0) index else ctx.nextByte()
      ctx.frame.stack = ctx.frame.locals(newIndex) :: ctx.frame.stack
    }
  }

  case class PushFromArray(id: Byte, name: String) extends StackOpCode{
    def width = 0
    def stackOp = { case (_, (index: Int) :: (arrayRef: Seq[Any]) :: baseStack) =>
      arrayRef(index) :: baseStack
    }
  }

  case class StoreArray(id: Byte, name: String) extends StackOpCode{
    def width = 0
    def stackOp = { case (_, value :: (index: Int) :: (arrayRef: Seq[Any]) :: baseStack ) =>
      arrayRef.updated(index, value) :: baseStack
    }
  }

  trait Branch extends OpCode{
    def op = { ctx =>
      val currentPc = ctx.frame.pc
      val (offset, newStack) = branchOp(ctx, ctx.stack)
      println("OldPC " + ctx.frame.pc)
      offset match{
        case Some(o) => ctx.frame.pc = currentPc + o - 1
        case None =>
      }

      ctx.frame.stack = newStack
      println("NewPC " + ctx.frame.pc)
    }

    def branchOp: (Context, List[Any]) => (Option[Int], List[Any])
  }

  case class UnaryBranch(id: Byte, name: String)(bop: Int => Boolean) extends Branch{
    def width = 2
    def branchOp = { case (ctx, (top: Int) :: baseStack) =>
      val offset = ctx.twoBytes()
      (if (bop(top)) Some(offset) else None , baseStack)
    }
  }
  case class BinaryBranch(id: Byte, name: String)(bop: (Int, Int) => Boolean) extends Branch{
    def width = 2
    def branchOp = { case (ctx, (top: Int) :: (next: Int) :: baseStack) =>
      val offset = ctx.twoBytes()
      println(bop(next, top) + " " + offset)
      (if (bop(next, top)) Some(offset) else None, baseStack)
    }
  }

  case class JsrBranch(id: Byte, name: String) extends Branch{
    def width = 2
    def branchOp = { case (ctx, (top: Int) :: (next: Int) :: baseStack) =>
      (Some(ctx.twoBytes()), baseStack)
    }
  }

  case class RetBranch(id: Byte, name: String) extends Branch{
    def width = 1
    def branchOp = { case (ctx, (top: Int) :: (second: Int) :: baseStack) =>
      (Some(ctx.frame.locals(ctx.nextByte()).asInstanceOf[Int]), baseStack)
    }
  }


}
