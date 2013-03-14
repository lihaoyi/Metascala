package sm

import collection.{GenSeq, mutable}
import imm._
import imm.Attached.LineNumber
import annotation.tailrec


trait Cache[In, Out] extends (In => Out){
  val cache = mutable.Map.empty[Any, Out]
  def pre(x: In): Any = x
  def calc(x: In): Out
  def post(y: Out): Unit = ()
  def apply(x: In) = {
    val newX = pre(x)
    cache.get(newX) match{
      case Some(y) => y
      case None =>
        val newY = calc(x)
        cache(newX) = newY
        post(newY)
        newY
    }
  }
}

object VM{
  var go = false
  def triggerGo() = ()
}
class VM(val natives: Natives = Natives.default, val log: ((=>String) => Unit)) {

  private[this] implicit val vm = this

  object InternedStrings extends Cache[virt.Obj, virt.Obj]{
    override def pre(x: virt.Obj) = virt.unvirtString(x)
    def calc(x: virt.Obj) = x
  }

  implicit object Types extends Cache[imm.Type, virt.Type]{
    def calc(t: imm.Type) = t match{
      case t: imm.Type.Cls => new virt.Cls(t)
      case _ => new virt.Type(t)
    }
  }

  implicit object Classes extends Cache[imm.Type.Cls, sm.Cls]{
    def calc(t: imm.Type.Cls): sm.Cls = {
      new sm.Cls(imm.Cls.parse(natives.fileLoader(t.name.replace(".", "/") + ".class").get))
    }
    override def post(cls: sm.Cls) = {
      cls.method("<clinit>", imm.Type.Desc.read("()V")).foreach( m =>
        threads(0).invoke(imm.Type.Cls(cls.name), "<clinit>", imm.Type.Desc.read("()V"), Nil)
      )
    }
  }

  lazy val threads = List(new VmThread())

  def invoke(bootClass: String, mainMethod: String, args: Seq[virt.Val]): virt.Val = {
    val res = threads(0).invoke(
      imm.Type.Cls(bootClass),
      mainMethod,
      imm.Type.Cls(bootClass).cls
        .clsData
        .methods
        .find(x => x.name == mainMethod)
        .map(_.desc)
        .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod)),
      args
    )

    res
  }
}




case class UncaughtVmException(name: String,
                               msg: String,
                               stackTrace: Seq[StackTraceElement],
                               stackData: Seq[FrameDump])
                               extends Exception(msg){

}
