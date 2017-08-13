package metascala

import java.io.Writer

import collection.mutable
import metascala.imm.{Sig, Type}
import metascala.rt.{Cls, ClsTable, Obj, Thread}
import metascala.natives.DefaultBindings
import metascala.opcodes.Conversion
import metascala.util._
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode



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
         initializeStdout: Boolean = false) extends Thread.VMInterface{
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
  def setOffHeapPointer(n: Long) = offHeapPointer = n
  val heap = new Heap(
    memorySize,
    () => getRoots(),
    getLinks
  )

  def checkInitialized(cls: rt.Cls): Unit = {
    if (!cls.initialized){
      cls.statics = vm.alloc(implicit i =>
        rt.Obj.allocArr(imm.Type.Prim.I, cls.staticList.length)
      )
      cls.initialized = true
      vm.resolveDirectRef(cls.tpe, Sig("<clinit>", imm.Desc.read("()V")))
        .foreach(threads(0).invoke(_, Agg.empty))

      cls.superType.foreach{ cls =>
        checkInitialized(vm.ClsTable(cls))
      }
    }
  }
  def alloc[T](func: Obj.Registrar => T): T = {
    val tempRegistry = mutable.Set[Ref]()
    val res = func(
      new Obj.Registrar({ ref =>

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
      rt.Obj.alloc(ClsTable(imm.Type.Cls("java/lang/Thread")),
        "group" -> rt.Obj.alloc(ClsTable(imm.Type.Cls("java/lang/ThreadGroup"))).address,
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
        rt.Obj.alloc(ClsTable(imm.Type.Cls("java/lang/Class")),
          "name" -> Virtualizer.toVirtObj(x.javaName)
        )
      )
    )
  }

  def invokeRun(a: Int) = {
    val pa = obj(a)
    val mRef = resolveDirectRef(pa.cls.tpe, pa.cls.methods.find(_.sig.name == "run").get.sig).get
    var x = 0
    threads(0).invoke(mRef, Agg(pa.address()))

    threads(0).returnedVal(0)
  }
  def lookupNatives(lookupName: String, lookupSig: imm.Sig) =
    vm.natives.trapped.find{case rt.NativeMethod(clsName, sig, func) =>
      (lookupName == clsName) && sig == lookupSig
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
      if cls.statics != null
    } yield cls.statics

    val classRoots2 = for {
      cls <- ClsTable.clsIndex.drop(1)
      if cls.statics != null
      i <- 0 until cls.staticList.length
      if cls.staticList(i).desc.isRef
    } yield new Ref.ArrRef(
      () => heap(cls.statics() + i + Constants.arrayHeaderSize),
      heap(cls.statics() + i + Constants.arrayHeaderSize) = _
    )

    val clsObjRoots = typeObjCache.values

    classRoots ++ classRoots2 ++ stackRoots ++ clsObjRoots ++ registry ++ interned
  }

  /**
    * Globally shared sun.misc.Unsafe object.
    */
  lazy val theUnsafe = vm.alloc(rt.Obj.alloc(ClsTable(imm.Type.Cls("sun/misc/Unsafe")))(_))

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
              new rt.ClsMethod(
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
    val systemCls = ClsTable.apply(imm.Type.Cls("java/lang/System"))
    checkInitialized(systemCls)
    val dummyWriter = vm.alloc(implicit r =>
      rt.Obj.alloc(ClsTable(imm.Type.Cls("java/io/PrintWriter")),
        "out" -> rt.Obj.alloc(ClsTable(imm.Type.Cls("metascala/DummyWriter"))).address
      ).address()
    )

    new rt.Arr(systemCls.statics)(systemCls.staticList.indexWhere(_.name == "out")) = dummyWriter
    new rt.Arr(systemCls.statics)(systemCls.staticList.indexWhere(_.name == "err")) = dummyWriter
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

