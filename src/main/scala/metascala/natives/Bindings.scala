package metascala.natives


import metascala.imm.{Sig, Type}
import metascala.rt.{Registrar, VMInterface0}
import metascala.{Agg, Ref, imm, rt}

import scala.collection.mutable
import scala.reflect.ClassTag
object Bindings{
  trait Interface extends VMInterface0{
    def invoke(cls: imm.Type.Cls, sig: Sig, args: Agg[Any]): Unit
    def invoke(mRef: rt.Method, args: Agg[Int]): Any
    def returnedVal: Array[Int]
    def alloc[T](func: Registrar => T): T
    def resolveDirectRef(owner: Type.Cls, sig: imm.Sig): Option[rt.Method]
    def typeObjCache: mutable.HashMap[imm.Type, Ref]
    def offHeap: Array[Byte]
    def setOffHeapPointer(n: Long): Unit
    def offHeapPointer: Long
    def arr(address: Int): rt.Arr
    def runningClassName(n: Int): String // vt.threadStack(n).runningClass.name
    def threadStackLength: Int // vt.threadStack.length
    def internedStrings: mutable.Map[String, Int]
    def toRealObj[T](x: Int)(implicit ct: ClassTag[T]): T
    def toVirtObj(x: Any)(implicit registrar: Registrar): rt.Obj
    def trace: Array[StackTraceElement]
    def currentThread: Int
  }
}
trait Bindings{


  val fileLoader: String => Option[Array[Byte]]



  val trapped: Seq[rt.Method]
}