import metascala.rt
import metascala.rt.{Arr, Obj, Thread}
import collection.mutable
import scala.reflect.ClassTag

package object metascala {
  import imm.Type.Prim

  /**
   * Represents a number referencing the local variable pool.
   */
  type Sym = Int

  class Registrar(f: Ref => Unit, val vm: VM) extends Function1[Ref, Unit]{
    def apply(i: Ref) = f(i)
  }

  implicit class pimpedAny(val x: Any) extends AnyVal{
    def toVirtObj(implicit registrar: Registrar) = {
      Virtualizer.pushVirtual(x).apply(0)
    }
  }
  def isObj(i: Int) = i < 0
  def isArr(i: Int) = i > 0
  implicit class pimpedVal(val v: Val) extends AnyVal{
    def isObj(implicit vm: VM) = metascala.isObj(vm.heap(v))
    def isArr(implicit vm: VM) = metascala.isArr(vm.heap(v))
    def obj(implicit vm: VM) = {

      //assert(isObj, v + " " + vm.heap.memory.slice(v / 10 * 10, v / 10 * 10 + 10).toList)
      new Obj(v)
    }
    def arr(implicit vm: VM) = {
//      assert(isArr, v)
      new Arr(v)
    }

    def toRealObj[T](implicit vm: VM, ct: ClassTag[T]) = {
      Virtualizer.popVirtual(imm.Type.Cls(ct.runtimeClass.getName.replace('.', '/')), () => v)
                 .asInstanceOf[T]
    }
  }

  trait Ref{
    def apply(): Int
    def update(i: Int): Unit
  }
  class ArrRef(val get: () => Int, val set: Int => Unit) extends Ref{
    def apply() = get()
    def update(i: Int) = set(i)
  }
  implicit class ManualRef(var x: Int) extends Ref{
    def apply() = x
    def update(i: Int) = x = i
  }

  type Val = Int



  implicit def stringToClass(s: String)(implicit vm: VM) = vm.ClsTable(imm.Type.Cls(s))
  implicit def stringToClsType(s: String) = imm.Type.Cls.apply(s)

  implicit class pimpedString(val s: String) extends AnyVal{

    def allocObj(initMembers: (String, Ref)*)(implicit registrar: Registrar) = {
      implicit val vm = registrar.vm
      rt.Obj.allocate(s, initMembers:_*).address
    }
    def allocArr(backing: TraversableOnce[Ref])(implicit registrar: Registrar) = {
      implicit val vm = registrar.vm
      rt.Arr.allocate(s, backing.toArray).address
    }
  }
  def reader(src: Seq[Val], index: Int) = {
    var i = index
    () => {
      i += 1
      src(i - 1)
    }
  }
  def writer(src: mutable.Seq[Val], index: Int) = {
    var i = index
    (x: Int) => {
      i += 1
      src(i - 1) = x
    }
  }
  def blit(src: Seq[Int], srcIndex: Int, dest: mutable.Seq[Int], destIndex: Int, length: Int) = {
    var i = 0
    while(i < length){
      dest(destIndex + i) = src(srcIndex + i)
      i+= 1
    }
  }

  def shorten(name: String) = {
    val some :+ last = name.split("/").toSeq
    (some.map(_(0)) :+ last).mkString("/")
  }
}
