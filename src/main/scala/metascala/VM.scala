package metascala

import collection.mutable
import annotation.tailrec
import metascala.imm.{Type, Code}
import rt.{FrameDump, Thread}
import metascala.natives.Bindings


/**
 * A Metascala VM. Call invoke() on it with a class, method name and arguments
 * to have it interpret some Java bytecode for you. It optionally takes in a set of
 * native bindings, as well as a logging function which it will use to log all of
 * its bytecode operations
 */
class VM(val natives: Bindings = Bindings.default, val log: ((=>String) => Unit) = s => ()) {
  private[this] implicit val vm = this

  object Heap{
    val memory = new Array[Int](4096)
    var freePointer = 1
    def allocate(n: Int) = {
      println("Allocating at " + freePointer)
      val newFree = freePointer
      freePointer += n
      newFree
    }
    def apply(n: Int) = memory(n)
    def update(n: Int, v: Int) = memory.update(n, v)

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

      val initMethod = cls.clsData
                          .methods
                          .find(m => m.name == "<clinit>" && m.desc == imm.Desc.read("()V"))

      initMethod.foreach( m => threads(0).invoke(cls.clsData.tpe, imm.Sig("<clinit>", imm.Desc.read("()V")), Nil))
    }
  }

  lazy val threads = List(new Thread())

  def invoke(bootClass: String, mainMethod: String, args: Seq[Any] = Nil): Any = {
    println(s"Invoking VM with $bootClass.$mainMethod")

    val res = threads(0).invoke(
      imm.Type.Cls(bootClass),
      imm.Sig(
        mainMethod,
        imm.Type.Cls(bootClass).cls
          .clsData
          .methods
          .find(x => x.name == mainMethod)
          .map(_.desc)
          .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod))
      ),
      args
    )
    res
  }


  println("Initialized VM")

  def resolveDirectRef(owner: Type.Cls, sig: imm.Sig)(implicit vm: VM): Option[rt.Method] = {
    val native =
      vm.natives
        .trapped
        .find(_.sig == sig)

    val method =
      owner.cls
        .methods
        .find(_.sig == sig)

    Some(
      native.orElse(method)
        .getOrElse(throw new Exception(s"Can't find method ${owner.unparse} ${sig.name} ${sig.desc.unparse}"))
    )


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