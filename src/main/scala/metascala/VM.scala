package metascala

import java.io.Writer

import collection.mutable
import annotation.tailrec
import metascala.imm.{Sig, Type}
import metascala.rt.{Cls, FrameDump, Obj, Thread}
import metascala.natives.DefaultBindings
import metascala.imm.Type.Prim
import metascala.opcodes.Conversion
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode


class DummyWriter extends Writer {
  def flush() = ()
  def close() = ()
  def write(cbuf: Array[Char], off: Val, len: Val) = ()
}


/**
 * A Metascala VM. Call invoke() on it with a class, method name and arguments
 * to have it interpret some Java bytecode for you. It optionally takes in a set of
 * native bindings, as well as a logging function which it will use to log all of
 * its bytecode operations
 */
class VM(val natives: DefaultBindings.type = DefaultBindings,
         val insnLimit: Long = Long.MaxValue,
         val log: ((=>String) => Unit) = s => (),
         val memorySize: Int = 1 * 1024 * 1024,
         initializeStdout: Boolean = false) extends VMInterface{
  def isObj(address: Int): Boolean = heap(address) < 0
  def isArr(address: Int): Boolean = heap(address) > 0
  def obj(address: Int): metascala.rt.Obj = new rt.Obj(address)
  def arr(address: Int): metascala.rt.Arr = new rt.Arr(address)
  def thUnsafe: metascala.rt.Obj = ???
  private[this] implicit val vm = this

  var ready = false
  val internedStrings = mutable.Map[String, Int]()

  // Doesn't grow for now; we can make it grow when we need it to.
  val offHeap = new Array[Byte](10)
  var offHeapPointer = 0L
  val heap = new Heap(
    memorySize,
    () => getRoots(),
    getLinks
  )

  def alloc[T](func: Registrar => T): T = {
    val tempRegistry = mutable.Set[Ref]()
    val res = func(
      new Registrar({ ref =>

        tempRegistry.add(ref)
        registry.add(ref)
      }, this)
    )
    tempRegistry.map(registry.remove)
    res
  }

  val registry = mutable.Set[Ref]()


  val arrayTypeCache = mutable.Buffer[imm.Type](null)

  lazy val currentThread = {
    val thread = alloc(implicit r =>
      rt.Obj.alloc("java/lang/Thread",
        "group" -> rt.Obj.alloc("java/lang/ThreadGroup").address,
        "priority" -> 5
      )
    ).address
    interned.append(thread)
    thread()
  }

  val interned = mutable.Buffer[Ref]()

  val typeObjCache = new mutable.HashMap[imm.Type, Ref] {
    override def apply(x: imm.Type) = this.getOrElseUpdate(x,
      vm.alloc(implicit r =>
        rt.Obj.alloc("java/lang/Class",
          "name" -> Virtualizer.toVirtObj(x.javaName)
        )
      )
    )
  }

  ready = true

  def getLinks(tpe: Int, length: Int): Seq[Int] = {
    if (isObj(tpe)) {
      for {
        (x, i) <- ClsTable.clsIndex(-heap(tpe)).fieldList.zipWithIndex
        if x.desc.isRef
      } yield i + Constants.objectHeaderSize
    } else {
      if (arrayTypeCache(heap(tpe)).isRef) {
        for (i <- 0 until heap(length)) yield i + Constants.arrayHeaderSize
      } else Nil
    }
  }

  /**
    * Identify the list of all root object references within the virtual machine.
    */
  def getRoots(): Seq[Ref] = {
    assert(ready)
    val stackRoots = for {
      thread <- threads
      frame <- thread.threadStack
      (blockId, index) = frame.pc
      block = frame.method.code.blocks(blockId)
      (x, i) <- block.locals.zipWithIndex
      if x.isRef
      _ = if (frame.locals(i) == -1) println(frame.locals.toList, i)
    } yield new Ref.ArrRef(() => frame.locals(i), frame.locals(i) = _)

    //    println(s"stackRoots ${stackRoots.map(_())}")

    val classRoots = for {
      cls <- ClsTable.clsIndex.drop(1)
    } yield cls.statics.address

    val classRoots2 = for {
      cls <- ClsTable.clsIndex.drop(1)
      i <- 0 until cls.staticList.length
      if cls.staticList(i).desc.isRef
    } yield new Ref.ArrRef(
      () => heap(cls.statics.address() + i + Constants.arrayHeaderSize),
      heap(cls.statics.address() + i + Constants.arrayHeaderSize) = _
    )

    val clsObjRoots = typeObjCache.values

    classRoots ++ classRoots2 ++ stackRoots ++ clsObjRoots ++ registry ++ interned

  }

  /**
    * Globally shared sun.misc.Unsafe object.
    */
  lazy val theUnsafe = vm.alloc(rt.Obj.alloc("sun/misc/Unsafe")(_))

  /**
    * Cache of all the classes loaded so far within the Metascala VM.
    */
  implicit object ClsTable extends ClsTable with Cache[imm.Type.Cls, rt.Cls] {
    val clsIndex = mutable.ArrayBuffer[rt.Cls](null)

    def calc(t: imm.Type.Cls): rt.Cls = {
      val input = natives.fileLoader(
        t.name + ".class"
      ).getOrElse(
        throw new Exception("Can't find " + t)
      )
      val cr = new ClassReader(input)
      val classNode = new ClassNode()
      cr.accept(classNode, ClassReader.EXPAND_FRAMES)

      Option(classNode.superName).map(Type.Cls.apply).map(vm.ClsTable)
      val fields = NullSafe(classNode.fields).map(imm.Field.read)
      val superType = NullSafe(classNode.superName).map(Type.Cls.apply)
      new Cls(
        tpe = imm.Type.Cls.apply(classNode.name),
        superType = superType,
        sourceFile = NullSafe(classNode.sourceFile),
        interfaces = NullSafe(classNode.interfaces).map(Type.Cls.apply),
        accessFlags = classNode.access,
        methods =
          NullSafe(classNode.methods)
            .zipWithIndex
            .map{case (mn, i) =>
              new rt.Method.Cls(
                clsIndex.length,
                i,
                Sig(mn.name, imm.Desc.read(mn.desc)),
                mn.access,
                () => Conversion.ssa(classNode.name, mn)
              )
            },
        fieldList =
          superType.toSeq.flatMap(ClsTable.apply(_).fieldList) ++
            fields.filter(!_.static).flatMap{x =>
              Seq.fill(x.desc.size)(x)
            },
        staticList =
          fields.filter(_.static).flatMap{x =>
            Seq.fill(x.desc.size)(x)
          },
        outerCls = NullSafe(classNode.outerClass).map(Type.Cls.apply),
        clsIndex.length
      )
    }

    var startTime = System.currentTimeMillis()

    override def post(cls: rt.Cls) = {
      clsIndex.append(cls)
    }
  }


  if (initializeStdout) {
    val systemCls = ClsTable.apply("java/lang/System")
    systemCls.checkInitialized()
    val dummyWriter = vm.alloc(implicit r =>
      rt.Obj.alloc("java/io/PrintWriter",
        "out" -> rt.Obj.alloc("metascala/DummyWriter").address
      ).address()
    )

    systemCls.statics(systemCls.staticList.indexWhere(_.name == "out")) = dummyWriter
    systemCls.statics(systemCls.staticList.indexWhere(_.name == "err")) = dummyWriter
  }
  def check(s: imm.Type, t: imm.Type): Boolean = {

    (s, t) match{

      case (s: imm.Type.Cls, t: imm.Type.Cls) => ClsTable(s).typeAncestry.contains(t)
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
      imm.Type.Cls.apply(bootClass),
      imm.Sig(
        mainMethod,
        ClsTable(imm.Type.Cls(bootClass))
          .methods
          .find(x => x.sig.name == mainMethod)
          .map(_.sig.desc)
          .getOrElse(throw new IllegalArgumentException("Can't find method: " + mainMethod))
      ),
      Agg.from(args)
    )
    res
  }

  def exec[T](thunk: => T): T = {
    val wrapped = () => thunk
    invoke(wrapped.getClass.getName, "apply", Seq(wrapped)).asInstanceOf[T]
  }
  println("Initialized VM")

  def resolveDirectRef(owner: Type.Cls, sig: imm.Sig): Option[rt.Method] = {

    val native =
      vm.natives
        .trapped
        .find(x => x.sig == sig && x.clsName == owner.name)

    val method =
      ClsTable(owner)
           .methods
           .find(_.sig == sig)


    native.orElse(method)
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