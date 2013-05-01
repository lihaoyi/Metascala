import metascala.rt.Thread
import collection.mutable
import scala.reflect.ClassTag

package object metascala {
  private[metascala] implicit class castable(val x: Any) extends AnyVal{
    def cast[T] = x.asInstanceOf[T]
  }
  implicit class pimpedAny(x: Any){
    def toVirtObj(implicit vm: VM) = {
      Virtualizer.pushVirtual(x).apply(0)
    }
  }
  implicit class pimpedVal(v: Val){
    def isObj(implicit vm: VM) = vm.Heap(v) < 0
    def isArr(implicit vm: VM) = vm.Heap(v) >= 0
    def obj(implicit vm: VM) = {
      assert(isObj)
      new vrt.Obj(v)
    }
    def arr(implicit vm: VM) = {
      assert(isArr)
      new vrt.Arr(v)
    }

    def toRealObj[T](implicit vm: VM, ct: ClassTag[T]) = {
      Virtualizer.popVirtual(imm.Type.Cls(ct.runtimeClass.getName.toSlash), () => v)
                 .cast[T]
    }
  }

  object Val{
    val Null = 0
    implicit def objToVal(x: vrt.Obj) = x.address
    implicit def arrToVal(x: vrt.Arr) = x.address
  }

  type Val = Int

  implicit class poppable(val vt: Thread) extends AnyVal{
    def pop = vt.frame.pop
    def popTo(dest: mutable.Seq[Val], index: Int, n: Int) = {
      for(i <- (n-1) to 0 by -1){
        dest(index + i) = pop
      }
    }
    def push(x: Val): Unit = vt.frame.push(x)
    def pushFrom(src: Seq[Val], index: Int, n: Int) = {
      for(i <- 0 until n){
        push(src(index + i))
      }
    }
    def popArgs(n: Int) = {
      val args = new Array[Val](n)
      popTo(args, 0, n)
      args
    }
  }

  implicit class pimpedString(val s: String){
    def toDot = s.replace('/', '.')
    def toSlash = s.replace('.', '/')
  }

  def forNameBoxed(name: String) = {
    if(Prim.all.contains(name(0)))
      Prim.all(name(0)).boxedClass
    else
      Class.forName(name)

  }
  def forName(name: String) = {
    if(Prim.all.contains(name(0)))
      Prim.all(name(0)).primClass
    else
      Class.forName(name)
  }

  def getAllFields(cls: Class[_]): Seq[java.lang.reflect.Field] = {
    Option(cls.getSuperclass)
      .toSeq
      .flatMap(getAllFields)
      .++(cls.getDeclaredFields)
  }

  implicit def stringToClass(s: String)(implicit vm: VM) = vm.ClsTable(imm.Type.Cls(s))
  implicit def stringToClsType(s: String) = imm.Type.Cls(s)
  implicit def stringToDesc(x: String) = imm.Desc.read(x)
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
}
