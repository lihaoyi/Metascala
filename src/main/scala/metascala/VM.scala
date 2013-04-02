package metascala

import collection.mutable
import annotation.tailrec
import imm.Code
import natives.Bindings
import rt.{FrameDump, Thread}


/**
 * A Metascala VM. Call invoke() on it with a class, method name and arguments
 * to have it interpret some Java bytecode for you. It optionally takes in a set of
 * native bindings, as well as a logging function which it will use to log all of
 * its bytecode operations
 */
class VM(val natives: Bindings = Bindings.default, val log: ((=>String) => Unit) = s => ()) {
  private[this] implicit val vm = this

  /**
   * Cache to keep track of interned vrt.Obj(java/lang/String)
   */
  object InternedStrings extends Cache[vrt.Obj, vrt.Obj]{
    override def pre(x: vrt.Obj) = vrt.unvirtString(x)
    def calc(x: vrt.Obj) = x
  }

  /**
   * Cache to keep track of vrt.Obj(java/lang/Class)
   */
  implicit object VirtualClassObjects extends Cache[imm.Type, vrt.Type]{
    def calc(t: imm.Type) = t match{
      case t: imm.Type.Cls => new vrt.Cls(t)
      case _ => new vrt.Type(t)
    }
  }

  /**
   * Globally shared sun.misc.Unsafe object.
   */
  lazy val theUnsafe = vrt.Obj("sun/misc/Unsafe")

  /**
   * Cache of all the classes loaded so far within the Metascala VM.
   */
  implicit object ClsTable extends Cache[imm.Type.Cls, rt.Cls]{
    val clsIndex = mutable.ArrayBuffer.empty[rt.Cls]
    def calc(t: imm.Type.Cls): rt.Cls = {
      val clsData = imm.Cls.parse(natives.fileLoader(t.name.replace(".", "/") + ".class").get)
      clsData.superType.map(vm.ClsTable)
      new rt.Cls(clsData, clsIndex.length)
    }
    var startTime = System.currentTimeMillis()
    override def post(cls: rt.Cls) = {
      clsIndex.append(cls)
      println("Initializing " + cls.clsData.tpe.unparse)
      println("" + ((System.currentTimeMillis() - startTime) / 1000))
      val initMethod = cls.clsData
                          .methods
                          .find(m => m.name == "<clinit>" && m.desc == imm.Desc.read("()V"))

      initMethod.foreach( m => threads(0).invoke(cls.clsData.tpe, "<clinit>", imm.Desc.read("()V"), Nil))
    }
  }

  lazy val threads = List(new Thread())

  def invoke(bootClass: String, mainMethod: String, args: Seq[vrt.Val] = Nil): Any = {
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

    vrt.unvirtualize(res)
  }


}

case class UncaughtVmException(name: String,
                               msg: String,
                               stackTrace: Seq[StackTraceElement],
                               stackData: Seq[FrameDump])
                               extends Exception(msg){

}

/**
 * A generic cache, which provides pre-processing of keys and post processing of values.
 */
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