package svm


object OpCodeGen {
  object OpCode{
    def unapply(o: OpCode) = (o.id, o.name, o.op)
    def apply(id: Int, name: String)(op: (Seq[Byte], VMThread) => VMThread) = BaseOpCode(id, name, op)
  }
  sealed trait OpCode{
    def id: Int
    def name: String
    def op: (Seq[Byte], VMThread) => VMThread
  }
  case class BaseOpCode(id: Int, name: String, op: (Seq[Byte], VMThread) => VMThread) extends OpCode

  case class StoreLocal(id: Int, name: String, index: Int) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack :+ last, rcp)) = x
      val newIndex = if (index > 0) index else inst(1)
      VMThread(pc, base :+ Frame(locals.updated(newIndex, last), operandStack, rcp))
    }
  }
  case class PushValOp(id: Int, name: String, ops: Seq[Byte] => Any) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack, rcp)) = x
      VMThread(pc, base :+ Frame(locals, operandStack :+ ops(inst), rcp))
    }
  }


  case class PushOp(id: Int, name: String, value: Any) extends StackManip{
    def stackOp = { _ :+ value }
  }

  case class pushConst(id: Int, name: String, pop: Seq[Byte] => Int) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack, rcp)) = x
      VMThread(pc, base :+ Frame(locals, operandStack :+ pop(inst), rcp))
    }
  }

  case class PushLocalIndexed(id: Int, name: String, index: Int) extends OpCode{
    def op = { (inst, x)=>
      val VMThread(pc, base :+ Frame(locals, operandStack, rcp)) = x
      val newIndex = if (index > 0) index else inst(1)
      VMThread(pc, base :+ Frame(locals, operandStack :+ locals(newIndex), rcp))
    }
  }

  case class PushFromArray(id: Int, name: String) extends StackManip{
    def stackOp = { x =>
      val stack :+ (arrayRef: Seq[Any]) :+ (index: Int) = x
      stack :+ arrayRef(index)
    }
  }
  case class  storeArray(id: Int, name: String) extends StackManip{
    def stackOp = { x =>
      val stack :+ (arrayRef: Seq[Any]) :+ (index: Int) :+ value = x
      stack :+ arrayRef.updated(index, value)
    }
  }
  object StackManip{
    def unapply(sm: StackManip) = (sm.id, sm.name, sm.stackOp)
    def apply(id: Int, name: String)(stackOp: Seq[Any] => Seq[Any]) = BaseStackManip(id, name, stackOp)

  }
  case class BaseStackManip(id: Int, name: String, stackOp: Seq[Any] => Seq[Any]) extends StackManip

  sealed trait StackManip extends OpCode{
    def stackOp: Seq[Any] => Seq[Any]
    def op = { (_, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack, rcp)) = x
      VMThread(pc, base :+ Frame(locals, stackOp(operandStack), rcp))
    }
  }


  case class UnaryBranch(id: Int, name: String)(bop: Int => Boolean) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack :+ (top: Int), rcp)) = x
      val offset = if (bop(top)){ inst(1) << 8 | inst(2) }else{ 0 }
      VMThread(pc + offset, base :+ Frame(locals, operandStack, rcp))
    }
  }
  case class BinaryBranch(id: Int, name: String)(bop: (Int, Int) => Boolean) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack :+ (second: Int) :+ (top: Int), rcp)) = x
      val offset = if (bop(second, top)){ inst(1) << 8 | inst(2) }else{ 0 }
      VMThread(pc + offset, base :+ Frame(locals, operandStack, rcp))
    }
  }

  case class JsrBranch(id: Int, name: String) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack :+ (second: Int) :+ (top: Int), rcp)) = x
      val offset = inst(1) << 8 | inst(2)
      VMThread(pc + offset, base :+ Frame(locals, operandStack :+ pc, rcp))
    }
  }

  case class RetBranch(id: Int, name: String) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack :+ (second: Int) :+ (top: Int), rcp)) = x
      val Seq(index) = inst
      VMThread(locals(index).asInstanceOf[Int], base :+ Frame(locals, operandStack :+ pc, rcp))
    }
  }

  case class ReturnBranch(id: Int, name: String) extends OpCode{
    def op = { (inst, x) =>
      val VMThread(pc, base :+ Frame(locals, operandStack :+ (second: Int) :+ (top: Int), rcp)) = x
      val Seq(index) = inst
      VMThread(locals(index).asInstanceOf[Int], base)
    }
  }
  case class ReturnStuff(id: Int, name: String) extends OpCode{
    def op = { (inst, x) =>
        val VMThread(pc, base :+ Frame(sLocals, sStack, rcp) :+ Frame(_, _ :+ top, _)) = x
        VMThread(pc, base :+ Frame(sLocals, sStack :+ top, rcp))
    }
  }
}
