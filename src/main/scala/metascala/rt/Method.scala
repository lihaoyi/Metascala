package metascala
package rt



/**
 * A reference to a method with a specific signature
 */
trait Method{
  def sig: imm.Sig
}
object Method{
  case class Native(clsName: String,
                     sig: imm.Sig,
                     func: rt.Thread => Unit)
                     extends Method{
    override def toString = s"Method.Native(${clsName}, ${sig.unparse}})"
  }


  /**
   * A reference to a method belonging to a class
   */
  case class Cls(cls: rt.Cls, methodIndex: Int, method: imm.Method)(implicit vm: VM) extends Method{
    lazy val sig = method.sig
    lazy val (blockMap, localsSize) = ssa.Conversion.convertToSsa(method)(vm.threads(0))
    lazy val blockIndexes =
      blockMap.toSeq
              .sortBy(_._1)
              .map(_._2.length)
              .scanLeft(0)(_+_)
    lazy val insns =
      blockMap.toSeq
              .sortBy(_._1)
              .flatMap(_._2)

    override def toString = s"Method.Cls(${cls.name}, ${method.sig.unparse}})"
  }
}
