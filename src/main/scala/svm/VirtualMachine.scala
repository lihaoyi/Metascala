package svm

import model.immutable.{ConstantInfo, ClassFile}

class VirtualMachine{
  val threads = List[VmThread](VmThread())
  var heap = Set.empty[model.mutable.Object]
  var classes = Map.empty[String, model.mutable.Class]
}

case class VmThread(
  var pc: Int = 0,
  var stack: List[Frame] = List()
)

class Frame(
  var locals: Seq[Any],
  var operandStack: List[Any],
  var constantPool: Seq[ConstantInfo]
)

