package sm

import collection.mutable
import annotation.tailrec
import imm.Code


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

  object InternedStrings extends Cache[vrt.Obj, vrt.Obj]{
    override def pre(x: vrt.Obj) = vrt.unvirtString(x)
    def calc(x: vrt.Obj) = x
  }

  implicit object Types extends Cache[imm.Type, vrt.Type]{
    def calc(t: imm.Type) = t match{
      case t: imm.Type.Cls => new vrt.Cls(t)
      case _ => new vrt.Type(t)
    }
  }
  lazy val theUnsafe = vrt.Obj("sun/misc/Unsafe")
  implicit object Classes extends Cache[imm.Type.Cls, rt.Cls]{
    val clsIndex = mutable.ArrayBuffer.empty[rt.Cls]
    def calc(t: imm.Type.Cls): rt.Cls = {
      val clsData = imm.Cls.parse(natives.fileLoader(t.name.replace(".", "/") + ".class").get)
      clsData.superType.map(vm.Classes)
      new rt.Cls(clsData, clsIndex.length)
    }
    var startTime = System.currentTimeMillis()
    override def post(cls: rt.Cls) = {
      clsIndex.append(cls)
      println("Initializing " + cls.clsData.tpe.unparse)
      println("" + ((System.currentTimeMillis() - startTime) / 1000))
      val initMethod = cls.clsData
                          .methods
                          .find(m => m.name == "<clinit>" && m.desc == imm.Type.Desc.read("()V"))

      println(initMethod)
      initMethod.foreach( m => threads(0).invoke(cls.clsData.tpe, "<clinit>", imm.Type.Desc.read("()V"), Nil))
    }
  }

  lazy val threads = List(new VmThread())

  def invoke(bootClass: String, mainMethod: String, args: Seq[vrt.Val]): vrt.Val = {
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
class BufferLog(n: Int) extends ((=> String) => Unit){
  val buffer = new Array[String](n)
  var index = 0
  var count = 0
  def apply(s: =>String) = {

    count += 1
    if (count > 10000000){
      buffer(index) = s
      index = (index + 1) % n
    }
  }
  def lines = buffer.drop(index) ++ buffer.take(index)
}