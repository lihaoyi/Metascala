package metascala

import org.objectweb.asm.tree.analysis.{BasicValue, Frame}

package object opcodes {
  implicit def unbox(b: Box) = b.value
  implicit class pimpedFrame(x: Frame[Box]){
    def top(n: Int = 0) = x.getStack(x.getStackSize - 1 - n)
    def boxes = {
      val locals = for {
        localId <- 0 until x.getLocals
        local <- Option(x.getLocal(localId))

      } yield if (local.getType != null) Some(local) else None
      val stackVals = for {
        stackId <- 0 until x.getStackSize
        stackVal = x.getStack(stackId)
      } yield Some(stackVal)
      locals ++ stackVals
    }
  }
}
