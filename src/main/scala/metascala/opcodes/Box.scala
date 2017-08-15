package metascala.opcodes

import org.objectweb.asm.tree.analysis.{BasicValue, Value}

class Box(val value: BasicValue) extends Value{
  override def getSize = value.getSize
  override def toString = Math.abs(hashCode).toString.take(2) + "" + value

}
