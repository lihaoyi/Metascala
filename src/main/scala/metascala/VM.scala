package metascala

import collection.mutable
import metascala.imm.{Sig, Type}
import metascala.rt.{Cls, ClsTable, Obj, Thread}
import metascala.natives.DefaultBindings
import metascala.opcodes.{LocalType, MethodSSAConverter}
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
         val memorySize: Int = 1 * 1024 * 1024,
         initializeStdout: Boolean = false,
         val logger: rt.Logger = /*NonLogger*/new ColorLogger {}) extends Thread.VMInterface{
  def isObj(address: Int): Boolean = heap(address) < 0
  def isArr(address: Int): Boolean = heap(address) > 0
  def obj(address: Int): metascala.rt.Obj = new rt.Obj(new Ref.UnsafeManual(address))
  def arr(address: Int): metascala.rt.Arr = new rt.Arr(new Ref.UnsafeManual(address))

  private[this] implicit val vm = this

  var ready = false
  val internedStrings = mutable.Map[String, WritableRef]()

  // Doesn't grow for now; we can make it grow when we need it to.
  val offHeap = new Array[Byte](10)
  var offHeapPointer = 0L
  def setOffHeapPointer(n: Long) = offHeapPointer = n
  val heap = new Heap(
    memorySize,
    forEachRoot,
    getLinks
  )

  def checkInitialized(cls: rt.Cls): Unit = {
    if (!cls.initialized){
      cls.statics = vm.alloc(r =>
        r.newArr(imm.Type.Prim.I, cls.staticList.length)
      )
      cls.initialized = true
      vm.resolveDirectRef(cls.tpe, Sig("<clinit>", imm.Desc.read("()V")))
        .foreach(threads(0).invoke(_, Agg.empty))

      cls.superType.foreach{ cls =>
        checkInitialized(vm.ClsTable(cls))
      }
    }
  }

  def alloc[T](func: rt.Allocator => T): T = {
    val allocator = new rt.Allocator()
    val link = activeAllocators.prepend(allocator)

    try func(allocator)
    finally activeAllocators.remove(link)
  }

  /**
    * Any allocators which are currently active, and thus serve as source roots
    * to stop the garbage collector from collecting partially-initialized
    * objects
    */
  var activeAllocators = new util.LinkedList[rt.Allocator]

  lazy val currentThread = {
    val thread = alloc( r =>
      r.newObj("java/lang/Thread",
        "group" -> r.newObj("java/lang/ThreadGroup").address,
        "priority" -> Ref.Raw(5)
      )
    ).address
    interned.append(thread)
    thread()
  }

  val interned = mutable.Buffer[WritableRef]()

  val typeObjCache = new mutable.HashMap[imm.Type, WritableRef] {
    override def apply(x: imm.Type) = this.getOrElseUpdate(x,
      vm.alloc(implicit r =>
        r.newObj("java/lang/Class",
          "name" -> r.register(Virtualizer.toVirtObj(x.javaName))
        )
      )
    )
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
      if (arr(tpe).innerType.isRef) {
        for (i <- 0 until heap(length)) yield i + Constants.arrayHeaderSize
      } else Nil
    }
  }

  /**
    * Identify the list of all root object references within the virtual machine.
    */
  def forEachRoot(cb: WritableRef => Unit): Unit = {
    assert(ready)
    for (thread <- threads; frame <- thread.threadStack){
      val (blockId, index) = frame.pc
      val block = frame.method.code.blocks(blockId)
      for((x, i) <- block.locals.zipWithIndex) {
        if (x == LocalType.Ref) {
          cb(new Ref.UnsafeArr(() => frame.locals(i), frame.locals(i) = _))
        }
      }
    }

    for (cls <- ClsTable.clsIndex) {
      if (cls != null && cls.statics != null) cb(cls.statics)
    }

    for (cls <- ClsTable.clsIndex){
      if (cls != null && cls.statics != null){

        for(i <- 0 until cls.staticList.length){
          if (cls.staticList(i).desc.isRef){
            cb(new Ref.UnsafeArr(
              () => heap(cls.statics() + i + Constants.arrayHeaderSize),
              heap(cls.statics() + i + Constants.arrayHeaderSize) = _
            ))
          }
        }
      }
    }

    typeObjCache.values.foreach(cb)

    for{
      allocator <- activeAllocators
      registered <- allocator.registered
    } cb(registered)

    interned.foreach(cb)

    internedStrings.valuesIterator.foreach(cb)
  }

  /**
    * Globally shared sun.misc.Unsafe object.
    */
  lazy val theUnsafe = vm.alloc(_.newObj("sun/misc/Unsafe"))

  /**
    * Cache of all the classes loaded so far within the Metascala VM.
    */
  implicit object ClsTable extends ClsTable {
    val clsIndex = mutable.ArrayBuffer[rt.Cls](null)
    val cache = mutable.Map.empty[imm.Type.Cls, rt.Cls]
    def apply(x: imm.Type.Cls) = {
      cache.get(x) match {
        case Some(y) => y
        case None =>
          val newY = calc(x)
          cache(x) = newY
          clsIndex.append(newY)
          newY
      }
    }
    def calc(t: imm.Type.Cls): rt.Cls = {
      val input = natives.fileLoader(
        t.name + ".class"
      ).getOrElse(
        throw new Exception("Can't find " + t)
      )

      calcFromBytes0(input)
    }

    def calcFromBytes(x: imm.Type.Cls, input: Array[Byte]): rt.Cls = {
      cache.get(x) match{
        case Some(y) => y
        case None =>
          val newY = calcFromBytes0(input)
          cache(x) = newY
          clsIndex.append(newY)
          newY
      }
    }
    def calcFromBytes0(input: Array[Byte]): rt.Cls = {
      val classNode = new ClassNode()
      val cr = new ClassReader(input)
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
                () => MethodSSAConverter.apply(classNode.name, mn)
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

  }


  if (initializeStdout) {
    val systemCls = ClsTable.apply("java/lang/System")
    checkInitialized(systemCls)
    val dummyWriter = vm.alloc( r =>
      r.newObj("java/io/PrintWriter",
        "out" -> r.newObj("metascala/DummyWriter").address
      ).address()
    )

    val sysProps = vm.alloc( r =>
      r.newObj("java/util/Properties").address()
    )

    new rt.Arr(systemCls.statics)(systemCls.staticList.indexWhere(_.name == "out")) = dummyWriter
    new rt.Arr(systemCls.statics)(systemCls.staticList.indexWhere(_.name == "err")) = dummyWriter
    new rt.Arr(systemCls.statics)(systemCls.staticList.indexWhere(_.name == "props")) = sysProps
    threads(0).invoke(
      ClsTable("java/util/Properties").method("<init>", imm.Desc(Agg.empty, imm.Type.Prim.V)).get,
      Agg(sysProps)
    )
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
        ClsTable(bootClass)
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

