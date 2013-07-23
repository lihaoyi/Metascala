import metascala.rt.Thread
import collection.mutable
import scala.reflect.ClassTag

package object metascala {
  import imm.Type.Prim

  /**
   * Represents a number referencing the local variable pool.
   */
  type Sym = Int

  private[metascala] implicit class castable(val x: Any) extends AnyVal{
    def cast[T] = x.asInstanceOf[T]
  }
  implicit class splitAllable[T](c: Seq[T]){
    def splitAll(positions: List[Int], index: Int = 0): Seq[Seq[T]] = positions match{
      case Nil => Seq(c)
      case firstPos :: restPos =>
        val (first, rest) = c.splitAt(firstPos - index)
        first +: rest.splitAll(restPos, firstPos)
    }
  }
  implicit class pimpedAny(x: Any){
    def toVirtObj(implicit vm: VM) = {
      Virtualizer.pushVirtual(x).apply(0)
    }
  }
  implicit class pimpedVal(v: Val){
    def isObj(implicit vm: VM) = vm.heap(v) < 0
    def isArr(implicit vm: VM) = vm.heap(v) >= 0
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
  implicit class pimpedString(val s: String){
    def toDot = s.replace('/', '.')
    def toSlash = s.replace('.', '/')
    def allocObj(initMembers: (String, Val)*)(implicit vm: VM) = {
      vrt.Obj.allocate(s, initMembers:_*).address
    }
    def allocArr(backing: Seq[Int])(implicit vm: VM) = {
      vrt.Arr.allocate(s, backing.toArray).address
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
}
