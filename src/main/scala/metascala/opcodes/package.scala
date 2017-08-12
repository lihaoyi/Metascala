package metascala

import org.objectweb.asm.tree.analysis.{BasicValue, Frame}

package object opcodes {
  implicit def unbox(b: Box) = b.value
  implicit class pimpedFrame(x: Frame[Box]){
    def top(n: Int = 0) = x.getStack(x.getStackSize - 1 - n)
    def boxes = {
      val locals = (0 until x.getLocals).map{ localId =>
        val local = x.getLocal(localId)
        if (local != null && local.getType != null) Some(local)
        else None
      }

      val stackVals =
        for (stackId <- 0 until x.getStackSize)
        yield Some(x.getStack(stackId))
      locals ++ stackVals
    }
  }
}
