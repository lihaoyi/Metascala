package metascala.natives


import metascala.imm.Sig
import metascala.rt.Obj
import metascala.util.{Agg, WritableRef}
import metascala.{imm, rt}

import scala.collection.mutable
import scala.reflect.ClassTag
object Bindings{
  trait Interface extends Obj.VMInterface{
    def throwExWithTrace(clsName: String, detailMessage: String): Unit
    def invoke0(cls: imm.Type.Cls, sig: Sig, args: Agg[Any]): Unit
    def invoke1(cls: imm.Type.Cls, sig: Sig, args: Agg[Int]): Unit

    def returnedVal: Array[Int]
    def alloc[T](func: rt.Allocator => T): T
    def typeObjCache: mutable.HashMap[imm.Type, WritableRef]
    def offHeap: Array[Byte]
    def setOffHeapPointer(n: Long): Unit
    def offHeapPointer: Long
    def arr(address: Int): rt.Arr
    def runningClassName(n: Int): String // vt.threadStack(n).runningClass.name
    def threadStackLength: Int // vt.threadStack.length
    def internedStrings: mutable.Map[String, WritableRef]
    def toRealObj[T](x: Int)(implicit ct: ClassTag[T]): T
    def toVirtObj(x: Any)(implicit registrar: rt.Allocator): rt.Obj
    def trace: Array[StackTraceElement]
    def currentThread: Int
    def invokeRun(a: Int): Int
    def newInstance(constr: Int, argArr: Int): Int
    def methodHandleMap: mutable.Map[WritableRef, rt.Method]
    def checkInitialized(cls: rt.Cls): Unit
    def getTypeForTypeObj(addr: Int): imm.Type
    def check(s: imm.Type, t: imm.Type): Boolean
    def boxIt(tpe: imm.Type.Prim[_], reader: () => Int)(implicit r: rt.Allocator): WritableRef
  }
}
trait Bindings{


  val fileLoader: String => Option[Array[Byte]]



  val trapped: Agg[rt.Method]
}