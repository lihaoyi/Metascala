package svm.model.opcodes

import org.objectweb.asm
import svm.model.{OpCode}
import svm.model.TypeDesc._
object LoadStore {
  case object Nop extends OpCode{
    def insnName = "nop"
    def id = 0
    def op = _ => ()
  }

  class PushOpCode(val id: Byte, val insnName: String, value: Any) extends OpCode{
    def op = _.frame.stack ::= value
  }

  case object AConstNull extends PushOpCode(1, "aconst_null", null)
  case object IConstNull extends PushOpCode(2, "iconst_m1", -1)

  case object IConst0 extends PushOpCode(3, "iconst_0", 0)
  case object IConst1 extends PushOpCode(4, "iconst_1", 1)
  case object IConst2 extends PushOpCode(5, "iconst_2", 2)
  case object IConst3 extends PushOpCode(6, "iconst_3", 3)
  case object IConst4 extends PushOpCode(7, "iconst_4", 4)
  case object IConst5 extends PushOpCode(8, "iconst_5", 5)

  case object LConst0 extends PushOpCode(9, "lconst_0", 0L)
  case object LConst1 extends PushOpCode(10, "lconst_1", 1L)

  case object FConst0 extends PushOpCode(11, "fconst_0", 0f)
  case object FConst1 extends PushOpCode(12, "fconst_1", 1f)
  case object FConst2 extends PushOpCode(13, "fconst_2", 2f)

  case object DConst0 extends PushOpCode(14, "dconst_0", 0d)
  case object DConst1 extends PushOpCode(15, "dconst_1", 1d)

  class PushValOpCode(val id: Byte, val insnName: String, value: Int) extends OpCode{
    def op = _.frame.stack ::= value
  }

  case class BiPush(value: Int) extends PushValOpCode(16, "bipush", value)
  case class SiPush(value: Int) extends PushValOpCode(17,"sipush", value)

  class PushConstOpCode(val id: Byte, val insnName: String, const: Any) extends OpCode{
    def op = ctx => {
      val newConst = const match{
        case s: String =>
          val x = new svm.Object(
            ctx.classes("java/lang/String"),
            ctx.classes
          )
          x.members("value") = s.toCharArray
          x
        case t: asm.Type =>
          val x = new svm.Object(
            ctx.classes("java/lang/Class"),
            ctx.classes
          )
          x.members("value") = t.getInternalName
          x
        case x => x
      }

      ctx.frame.stack ::= newConst
    }
  }

  case class Ldc(const: Any) extends PushConstOpCode(18, "ldc", const)

  // Not used, because ASM converts these Ldc(const: Any)
  //===============================================================
  case class LdcW(const: Any) extends UnusedOpCode(19, "ldc_w")
  case class Ldc2W(const: Any) extends UnusedOpCode(20, "ldc2_w")
  //===============================================================

  abstract class PushLocalIndexed(val id: Byte, val insnName: String) extends OpCode{
    def op = (ctx => ctx.frame.stack ::= ctx.frame.locals(index))
    def index: Int
  }

  case class ILoad(index: Int) extends PushLocalIndexed(21, "iLoad")
  case class LLoad(index: Int) extends PushLocalIndexed(22, "lLoad")
  case class FLoad(index: Int) extends PushLocalIndexed(23, "fLoad")
  case class DLoad(index: Int) extends PushLocalIndexed(24, "dLoad")
  case class ALoad(index: Int) extends PushLocalIndexed(25, "aLoad")



  // Not used, because ASM converts these to raw XLoad(index: Int)s
  //===============================================================
  case class ILoad0(index: Int) extends UnusedOpCode(26, "iLoad_0")
  case class ILoad1(index: Int) extends UnusedOpCode(27, "iLoad_1")
  case class ILoad2(index: Int) extends UnusedOpCode(28, "iLoad_2")
  case class ILoad3(index: Int) extends UnusedOpCode(29, "iLoad_3")

  case class LLoad0(index: Int) extends UnusedOpCode(30, "lLoad_0")
  case class LLoad1(index: Int) extends UnusedOpCode(31, "lLoad_1")
  case class LLoad2(index: Int) extends UnusedOpCode(32, "lLoad_2")
  case class LLoad3(index: Int) extends UnusedOpCode(33, "lLoad_3")

  case class FLoad0(index: Int) extends UnusedOpCode(34, "fLoad_0")
  case class FLoad1(index: Int) extends UnusedOpCode(35, "fLoad_1")
  case class FLoad2(index: Int) extends UnusedOpCode(36, "fLoad_2")
  case class FLoad3(index: Int) extends UnusedOpCode(37, "fLoad_3")

  case class DLoad0(index: Int) extends UnusedOpCode(38, "dLoad_0")
  case class DLoad1(index: Int) extends UnusedOpCode(39, "dLoad_1")
  case class DLoad2(index: Int) extends UnusedOpCode(40, "dLoad_2")
  case class DLoad3(index: Int) extends UnusedOpCode(41, "dLoad_3")

  case class ALoad0(index: Int) extends UnusedOpCode(42, "aLoad_0")
  case class ALoad1(index: Int) extends UnusedOpCode(43, "aLoad_1")
  case class ALoad2(index: Int) extends UnusedOpCode(44, "aLoad_2")
  case class ALoad3(index: Int) extends UnusedOpCode(45, "aLoad_3")
  //===============================================================

  class PushFromArray[T](val id: Byte, val insnName: String) extends OpCode{
    def op = ctx => {
      val Intish(index) :: (array: Array[T]) :: stack = ctx.stack
      if (array.isDefinedAt(index))
        ctx.frame.stack = array(index) :: stack
      else{
        ctx.throwException{
          val ex = new svm.Object(ctx classes "java/lang/ArrayIndexOutOfBoundsException", ctx.classes)
          ex.members("detailMessage") = svm.Object.toVirtual(index+"")(ctx.classes)
          ex
        }
      }
    }
  }
  class PushFromArrayInt[T: Numeric](val id: Byte, val insnName: String) extends OpCode{
    def op = ctx => {
      val Intish(index) :: (array: Array[T]) :: stack = ctx.stack
      if (array.isDefinedAt(index))
        ctx.frame.stack = implicitly[Numeric[T]].toInt(array(index)) :: stack
      else{
        ctx.throwException{
          val ex = new svm.Object(ctx classes "java/lang/ArrayIndexOutOfBoundsException", ctx.classes)
          ex.members("detailMessage") = svm.Object.toVirtual(index+"")(ctx.classes)
          ex
        }
      }
    }
  }
  case object IALoad extends PushFromArray[Int](46, "iaLoad")
  case object LALoad extends PushFromArray[Long](47, "laLoad")
  case object FALoad extends PushFromArray[Float](48, "faLoad")
  case object DALoad extends PushFromArray[Double](49, "daLoad")
  case object AALoad extends PushFromArray[Object](50, "aaLoad")
  case object BALoad extends PushFromArrayInt[Byte](51, "baLoad")
  case object CALoad extends PushFromArrayInt[Char](52, "caLoad")
  case object SALoad extends PushFromArrayInt[Short](53, "saLoad")

  abstract class StoreLocal(val id: Byte, val insnName: String) extends OpCode{
    def varId: Int
    def op = ctx => ctx.swapStack{ case top :: stack =>
      ctx.frame.locals(varId) = top
      stack
    }
  }
  case class IStore(varId: Int) extends StoreLocal(54, "istore")
  case class LStore(varId: Int) extends StoreLocal(55, "lstore")
  case class FStore(varId: Int) extends StoreLocal(56, "fstore")
  case class DStore(varId: Int) extends StoreLocal(57, "dstore")
  case class AStore(varId: Int) extends StoreLocal(58, "astore")

  // Not used, because ASM converts these to raw XStore(index: Int)s
  //===============================================================
  case class IStore0(varId: Int) extends UnusedOpCode(59, "istore_0")
  case class IStore1(varId: Int) extends UnusedOpCode(60, "istore_1")
  case class IStore2(varId: Int) extends UnusedOpCode(61, "istore_2")
  case class IStore3(varId: Int) extends UnusedOpCode(62, "istore_3")

  case class LStore0(varId: Int) extends UnusedOpCode(63, "lstore_0")
  case class LStore1(varId: Int) extends UnusedOpCode(64, "lstore_1")
  case class LStore2(varId: Int) extends UnusedOpCode(65, "lstore_2")
  case class LStore3(varId: Int) extends UnusedOpCode(66, "lstore_3")

  case class FStore0(varId: Int) extends UnusedOpCode(67, "fstore_0")
  case class FStore1(varId: Int) extends UnusedOpCode(68, "fstore_1")
  case class FStore2(varId: Int) extends UnusedOpCode(69, "fstore_2")
  case class FStore3(varId: Int) extends UnusedOpCode(70, "fstore_3")

  case class DStore0(varId: Int) extends UnusedOpCode(71, "dstore_0")
  case class DStore1(varId: Int) extends UnusedOpCode(72, "dstore_1")
  case class DStore2(varId: Int) extends UnusedOpCode(73, "dstore_2")
  case class DStore3(varId: Int) extends UnusedOpCode(74, "dstore_3")

  case class AStore0(varId: Int) extends UnusedOpCode(75, "astore_0")
  case class AStore1(varId: Int) extends UnusedOpCode(76, "astore_1")
  case class AStore2(varId: Int) extends UnusedOpCode(77, "astore_2")
  case class AStore3(varId: Int) extends UnusedOpCode(78, "astore_3")
  //===============================================================

  class StoreArray[T](val id: Byte, val insnName: String) extends OpCode{
    def op = _.swapStack{
      case (value: T) :: Intish(index) :: (array: Array[T]) :: stack =>
        array(index) = value
        stack
    }
  }
  class StoreArrayInt[T](val id: Byte, val insnName: String)(x: Int => T) extends OpCode{
    def op = _.swapStack {
      case (value: Int) :: Intish(index) :: (array: Array[T]) :: stack =>
        array(index) = x(value)
        stack
    }
  }
  case object IAStore extends StoreArray[Int](79, "iastore")
  case object LAStore extends StoreArray[Long](80, "lastore")
  case object FAStore extends StoreArray[Float](81, "fastore")
  case object DAStore extends StoreArray[Double](82, "dastore")
  case object AAStore extends StoreArray[Object](83, "aastore")
  case object BAStore extends StoreArrayInt[Byte](84, "bastore")(_.toByte)
  case object CAStore extends StoreArrayInt[Char](85, "castore")(_.toChar)
  case object SAStore extends StoreArrayInt[Short](86, "sastore")(_.toShort)
}
