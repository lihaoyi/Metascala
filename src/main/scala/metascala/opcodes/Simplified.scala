package metascala
package opcodes

import LoadStore._
import StackManip._
import Misc._
object Simplified {
  /**
   * Potential future register-based opcodes
   */
  object Future{
    type Local = Int
    case class Set[T](p: Prim[T])(dest: Local, value: T)
    case class UnaryOp[T](p: Prim[T])(a: Local, func: T => T)
    case class BinaryOp[T](p: Prim[T])(a: Local, localBb: Local, func: (T, T) => T)
    case class Branch(src: Local, target: Int)
    case class LoadArray(src: Local, dest: Local, size: Int)
    case class LoadStatic(src: Local, target: Int, field: Int, size: Int)
    case class LoadField(src: Local, target: Local, field: Int, size: Int)
    case class StoreArray(local: Local, target: Local, size: Int)
    case class StoreStatic(local: Local, target: Local, field: Int, size: Int)
    case class StoreField(local: Local, target: Local, field: Int, size: Int)
  }
  /**
   * Simplified set of all current opcodes
   */
  Nop
  
  Const
  Push
  Ldc
  
  Load
  Store
  
  ManipStack
  
  BinOp
  UnaryOp
  IInc
  
  UnaryBranch
  BinaryBranch
  Goto

  ReturnVal
  
  LoadArray
  StoreArray
  GetStatic
  PutStatic
  GetField
  PutField
  ArrayLength
  
  InvokeVirtual
  InvokeSpecial
  InvokeStatic
  InvokeInterface
  InvokeDynamic
  
  New
  NewArray
  ANewArray
  MultiANewArray

  AThrow
  CheckCast
  InstanceOf
  MonitorEnter
  MonitorExit
  
}
