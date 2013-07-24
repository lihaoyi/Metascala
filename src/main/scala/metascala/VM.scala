package metascala

import collection.mutable
import annotation.tailrec
import metascala.imm.{Type, Code}
import rt.{FrameDump, Thread}
import metascala.natives.Bindings
import metascala.imm.Type.Prim


class ArrRef(backing: Array[Int], index: Int){
  def update(i: Int) = backing(index) = i
  def apply() = backing(index)
}
/**
 * A Metascala VM. Call invoke() on it with a class, method name and arguments
 * to have it interpret some Java bytecode for you. It optionally takes in a set of
 * native bindings, as well as a logging function which it will use to log all of
 * its bytecode operations
 */
class VM(val natives: Bindings = Bindings.default,
         val log: ((=>String) => Unit) = s => (),
         val memorySize: Int = 1 * 1024 * 1024) {

  private[this] implicit val vm = this

  val internedStrings = mutable.Map[String, Int]()
  val heap = new Heap(memorySize)
  val arrayTypeCache = mutable.Buffer.empty[imm.Type]

  /**
   * Identify the list of all root object references within the virtual machine.
   */
  def getRoots() = {
    val stackRoots = for{
      thread <- threads
//      _ = println("threadStack " + thread.threadStack.map(_.runningClass.name))
      frame <- thread.threadStack
      (blockId, index) = frame.pc
      block = frame.method.code.blocks(blockId)
      _ = println(frame.method.method.name + "\t" + frame.locals.zip(block.locals).toList)
      _ = println(frame.locals.toList)
      _ = println(block.locals.toList)
      (x, i) <- block.locals.zipWithIndex

      if x.isRef
    } yield new ArrRef(frame.locals, i)

//    println(s"stackRoots ${stackRoots.map(_())}")

    val classRoots = for{
      cls <- ClsTable.clsIndex.drop(1)
//      _ = println("Cls " + cls.name)
      (field, i) <- cls.staticList.zipWithIndex
//      _ = println("Field " + field.name + "\t" + cls.statics(i))
      if field.desc.isRef
    } yield new ArrRef(cls.statics, i)

//    println(s"classRoots ${classRoots.map(_())}")

    stackRoots ++ classRoots
  }
  /**
   * Globally shared sun.misc.Unsafe object.
   */
  lazy val theUnsafe = vrt.Obj.allocate("sun/misc/Unsafe")

  /**
   * Cache of all the classes loaded so far within the Metascala VM.
   */
  implicit object ClsTable extends Cache[imm.Type.Cls, rt.Cls]{
    val clsIndex = mutable.ArrayBuffer[rt.Cls](null)

    def calc(t: imm.Type.Cls): rt.Cls = {
      val clsData = imm.Cls.parse(
        natives.fileLoader(t.name.replace(".", "/") + ".class").getOrElse(
          throw new Exception("Can't find " + t)
        )
      )
      clsData.superType.map(vm.ClsTable)
      new rt.Cls(clsData, clsIndex.length)
    }

    var startTime = System.currentTimeMillis()

    override def post(cls: rt.Cls) = {
      clsIndex.append(cls)

    }
  }

  def check(s: imm.Type, t: imm.Type): Boolean = {

    (s, t) match{

      case (s: imm.Type.Cls, t: imm.Type.Cls) => s.cls.typeAncestry.contains(t)
      case (s: imm.Type.Arr, imm.Type.Cls("java/lang/Object")) => true
      case (s: imm.Type.Arr, imm.Type.Cls("java/lang/Cloneable")) => true
      case (s: imm.Type.Arr, imm.Type.Cls("java/io/Serializable")) => true
      case (imm.Type.Arr(imm.Type.Prim(a)), imm.Type.Arr(imm.Type.Prim(b))) => a == b
      case (imm.Type.Arr(sc: imm.Type), imm.Type.Arr(tc: imm.Type)) => check(sc, tc)
      case _ => false
    }
  }

  lazy val threads = List(new Thread())

  def invoke(bootClass: String, mainMethod: String, args: Seq[Any] = Nil): Any = {
    println(s"Invoking VM with $bootClass.$mainMethod")

    val res = threads(0).invoke(
      imm.Type.Cls(bootClass),
      imm.Sig(
        mainMethod,
        imm.Type.Cls(bootClass)
          .cls
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
        .find(x => x.sig == sig && x.clsName == owner.name)

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

class WrappedVmException(wrapped: Throwable) extends Exception(wrapped)
case class UncaughtVmException(wrapped: Throwable) extends WrappedVmException(wrapped)
case class InternalVmException(wrapped: Throwable) extends WrappedVmException(wrapped)

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