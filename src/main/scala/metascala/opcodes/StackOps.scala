package metascala

import StackOps._
import rt.Thread
import org.objectweb.asm.Label
import org.objectweb.asm.tree._
import collection.convert.wrapAsScala._
import metascala.imm.Type

/**
 * `StackOps` contains the stack manipulating behavior of each individual
 * opcode. Each opcode is a case class or case object extending the trait
 * [[metascala.StackOps.OpCode]]. These are split into three separate files to help keep
 * compile times down.
 *
 * A large number of the StackOps are unused (they extend [[metascala.StackOps.UnusedOpCode]])
 * as ASM folds these into other StackOps for us automatically. For example,
 * `LDC`, `LDC_W` and `LDC_2W` all get folded into `LDC` by ASM before being
 * made available to Metascala. Furthermore, some StackOps are immediately
 * converted into optimized variants living in Optimized.scala, for example with
 * pre-computed method or field offsets.
 */
object StackOps {

  abstract class OpCode

  object OpCode



  private[this] implicit class nullSafeList[T](val list: java.util.List[T]) extends AnyVal{
    def safeList: Seq[T] = {
      Option(list).toVector.flatten
    }
  }

  def read(implicit labelMap: Map[Label, Int]): PartialFunction[Any, OpCode] = {
    case x: FieldInsnNode           => all(x.getOpcode).cast[(imm.Type.Cls, String, imm.Type) => OpCode].apply(x.owner, x.name, imm.Type.read(x.desc))
    case x: IincInsnNode            => all(x.getOpcode).cast[(Int, Int) => OpCode].apply(x.`var`, x.incr)
    case x: InsnNode                => all(x.getOpcode).cast[OpCode]
    case x: IntInsnNode             => all(x.getOpcode).cast[Int => OpCode].apply(x.operand)
    case x: InvokeDynamicInsnNode   => all(x.getOpcode).cast[(String, String, Object, Object) => OpCode].apply(x.name, x.desc, x.bsm, x.bsmArgs)
    case x: JumpInsnNode            => all(x.getOpcode).cast[Int => OpCode].apply(x.label.getLabel)
    case x: LdcInsnNode             => all(x.getOpcode).cast[Object => OpCode].apply(x.cst)
    case x: LookupSwitchInsnNode    => all(x.getOpcode).cast[(Int, Seq[Int], Seq[Int]) => OpCode].apply(x.dflt.getLabel, x.keys.safeList.map(x => x: Int), x.labels.safeList.map(x => labelMap(x.getLabel)))
    case x: MethodInsnNode          => all(x.getOpcode).cast[(imm.Type, imm.Sig) => OpCode].apply(imm.Type.read(x.owner), imm.Sig(x.name, x.desc))
    case x: MultiANewArrayInsnNode  => all(x.getOpcode).cast[(imm.Type, Int) => OpCode].apply(imm.Type.read(x.desc), x.dims)
    case x: TableSwitchInsnNode     => all(x.getOpcode).cast[(Int, Int, Int, Seq[Int]) => OpCode].apply(x.min, x.max, x.dflt.getLabel, x.labels.safeList.map(x => labelMap(x.getLabel)))
    case x: TypeInsnNode            => all(x.getOpcode).cast[imm.Type => OpCode].apply(imm.Type.read(x.desc))
    case x: VarInsnNode             => all(x.getOpcode).cast[Int => OpCode].apply(x.`var`)
  }


  case object Nop extends OpCode{
    def op(vt: Thread) = ()
  }

  case class Const[A](b: Prim[A])(val value: A)(name: String) extends OpCode{
    def words = {
      var out = List[Int]()
      b.write(value, out ::= _)
      out
    }
  }

  val AConstNull = Const(I)(0)("AConstNull")
  val IConstM1 = Const(I)(-1)("IConstM1")

  val IConst0 = Const(I)(0)("IConst0")
  val IConst1 = Const(I)(1)("IConst1")
  val IConst2 = Const(I)(2)("IConst2")
  val IConst3 = Const(I)(3)("IConst3")
  val IConst4 = Const(I)(4)("IConst4")
  val IConst5 = Const(I)(5)("IConst5")

  val LConst0 = Const(J)(0)("LConst0")
  val LConst1 = Const(J)(1)("LConst1")

  val FConst0 = Const(F)(0)("FConst0")
  val FConst1 = Const(F)(1)("FConst1")
  val FConst2 = Const(F)(2)("FConst2")

  val DConst0 = Const(D)(0)("DConst0")
  val DConst1 = Const(D)(1)("DConst1")

  case class Push(value: Int)(override val toString: String) extends OpCode
  val BiPush = Push(_: Int)("BiPush")
  val SiPush = Push(_: Int)("SiPush")


  case class Ldc(const: Any) extends OpCode

  // Not used, because ASM converts these Ldc(const: Any)
  //===============================================================
  val LdcW = UnusedOpCode
  val Ldc2W = UnusedOpCode
  //===============================================================


  case class Load[T](index: Int, p: Prim[T])(name: String) extends OpCode

  val ILoad = Load(_: Int, I)("ILoad")
  val LLoad = Load(_: Int, J)("LLoad")
  val FLoad = Load(_: Int, F)("FLoad")
  val DLoad = Load(_: Int, D)("DLoad")
  val ALoad = Load(_: Int, I)("ALoad")



  // Not used, because ASM converts these to raw XLoad(index: Int)s
  //===============================================================
  val ILoad0 = UnusedOpCode
  val ILoad1 = UnusedOpCode
  val ILoad2 = UnusedOpCode
  val ILoad3 = UnusedOpCode

  val LLoad0 = UnusedOpCode
  val LLoad1 = UnusedOpCode
  val LLoad2 = UnusedOpCode
  val LLoad3 = UnusedOpCode

  val FLoad0 = UnusedOpCode
  val FLoad1 = UnusedOpCode
  val FLoad2 = UnusedOpCode
  val FLoad3 = UnusedOpCode

  val DLoad0 = UnusedOpCode
  val DLoad1 = UnusedOpCode
  val DLoad2 = UnusedOpCode
  val DLoad3 = UnusedOpCode

  val ALoad0 = UnusedOpCode
  val ALoad1 = UnusedOpCode
  val ALoad2 = UnusedOpCode
  val ALoad3 = UnusedOpCode
  //===============================================================

  case class LoadArray[T](p: Prim[T])(override val toString: String) extends OpCode
  val IALoad = LoadArray(I)("IALoad")
  val LALoad = LoadArray(J)("LALoad")
  val FALoad = LoadArray(F)("FALoad")
  val DALoad = LoadArray(D)("DALoad")
  val AALoad = LoadArray(I)("AALoad")
  val BALoad = LoadArray(B)("BALoad")
  val CALoad = LoadArray(C)("CALoad")
  val SALoad = LoadArray(S)("SALoad")

  case class Store[T](index: Int, p: Prim[T])(name: String) extends OpCode

  val IStore = Store(_: Int, I)("IStore")
  val LStore = Store(_: Int, J)("LStore")
  val FStore = Store(_: Int, F)("FStore")
  val DStore = Store(_: Int, D)("DStore")
  val AStore = Store(_: Int, I)("AStore")

  // Not used, because ASM converts these to raw XStore(index: Int)s
  //===============================================================
  val IStore0 = UnusedOpCode
  val IStore1 = UnusedOpCode
  val IStore2 = UnusedOpCode
  val IStore3 = UnusedOpCode

  val LStore0 = UnusedOpCode
  val LStore1 = UnusedOpCode
  val LStore2 = UnusedOpCode
  val LStore3 = UnusedOpCode

  val FStore0 = UnusedOpCode
  val FStore1 = UnusedOpCode
  val FStore2 = UnusedOpCode
  val FStore3 = UnusedOpCode

  val DStore0 = UnusedOpCode
  val DStore1 = UnusedOpCode
  val DStore2 = UnusedOpCode
  val DStore3 = UnusedOpCode

  val AStore0 = UnusedOpCode
  val AStore1 = UnusedOpCode
  val AStore2 = UnusedOpCode
  val AStore3 = UnusedOpCode
  //===============================================================


  case class StoreArray[T](p: Prim[T])(override val toString: String) extends OpCode
  val IAStore = StoreArray(I)("IAStore")
  val LAStore = StoreArray(J)("LAStore")
  val FAStore = StoreArray(F)("FAStore")
  val DAStore = StoreArray(D)("DAStore")
  val AAStore = StoreArray(I)("AAStore")
  val BAStore = StoreArray(B)("BAStore")
  val CAStore = StoreArray(C)("CAStore")
  val SAStore = StoreArray(S)("SAStore")

  case class ManipStack(transform: List[Any] => List[Any])(override val toString: String) extends OpCode
  val Pop = ManipStack{ case _ :: s => s }("Pop")
  val Pop2 = ManipStack{ case _ :: _ :: s => s }("Pop2")
  val Dup = ManipStack{ case top :: s => top :: top :: s }("Dup")
  val DupX1 = ManipStack{ case top :: x :: s => top :: x :: top :: s }("DupX1")
  val DupX2 = ManipStack{ case top :: y :: x :: s => top :: y :: x :: top :: s }("DupX2")
  val Dup2 = ManipStack{ case y :: x :: s => y :: x :: y :: x :: s }("Dup2")
  val Dup2X1 = ManipStack{ case a :: b :: x :: s => a :: b :: x :: a :: b :: s }("Dup2X1")
  val Dup2X2 = ManipStack{ case a :: b :: x :: y :: s => a :: b :: x :: y :: a :: b :: s }("Dup2X2")
  val Swap = ManipStack{ case x :: y :: s=> y :: x :: s }("Swap")


  val IAdd = BinOp(I, I, I)(_ + _)("IAdd")
  val LAdd = BinOp(J, J, J)(_ + _)("LAdd")
  val FAdd = BinOp(F, F, F)(_ + _)("FAdd")
  val DAdd = BinOp(D, D, D)(_ + _)("DAdd")

  val ISub = BinOp(I, I, I)(_ - _)("ISub")
  val LSub = BinOp(J, J, J)(_ - _)("LSub")
  val FSub = BinOp(F, F, F)(_ - _)("FSub")
  val DSub = BinOp(D, D, D)(_ - _)("DSub")

  val IMul = BinOp(I, I, I)(_ * _)("IMul")
  val LMul = BinOp(J, J, J)(_ * _)("LMul")
  val FMul = BinOp(F, F, F)(_ * _)("FMul")
  val DMul = BinOp(D, D, D)(_ * _)("DMul")

  val IDiv = BinOp(I, I, I)(_ / _)("IDiv")
  val LDiv = BinOp(J, J, J)(_ / _)("LDiv")
  val FDiv = BinOp(F, F, F)(_ / _)("FDiv")
  val DDiv = BinOp(D, D, D)(_ / _)("DDiv")

  val IRem = BinOp(I, I, I)(_ % _)("IRem")
  val LRem = BinOp(J, J, J)(_ % _)("LRem")
  val FRem = BinOp(F, F, F)(_ % _)("FRem")
  val DRem = BinOp(D, D, D)(_ % _)("DRem")

  val INeg = UnaryOp(I, I)(-_)("INeg")
  val LNeg = UnaryOp(J, J)(-_)("LNeg")
  val FNeg = UnaryOp(F, F)(-_)("FNeg")
  val DNeg = UnaryOp(D, D)(-_)("DNeg")

  val IShl = BinOp(I, I, I)(_ << _)("IShl")
  val LShl = BinOp(I, J, J)(_ << _)("LShl")
  val IShr = BinOp(I, I, I)(_ >> _)("IShr")
  val LShr = BinOp(I, J, J)(_ >> _)("LShr")

  val IUShr = BinOp(I, I, I)(_ >>> _)("IUShr")
  val LUShr = BinOp(I, J, J)(_ >>> _)("LUShr")

  val IAnd = BinOp(I, I, I)(_ & _)("IAnd")
  val LAnd = BinOp(J, J, J)(_ & _)("LAnd")

  val IOr = BinOp(I, I, I)(_ | _)("IOr")
  val LOr = BinOp(J, J, J)(_ | _)("LOr")

  val IXOr = BinOp(I, I, I)(_ ^ _)("IXOr")
  val LXOr = BinOp(J, J, J)(_ ^ _)("LXOr")

  case class IInc(varId: Int, amount: Int) extends OpCode

  case class UnaryOp[A, R](a: Prim[A], out: Prim[R])
                          (val func: A => R)(override val toString: String) extends OpCode

  case class BinOp[A, B, R](a: Prim[A], b: Prim[B], out: Prim[R])
                           (val func: (B, A) => R)
                           (override val toString: String)extends OpCode

  val I2L = UnaryOp(I, J)(_.toLong)  ("I2L")
  val I2F = UnaryOp(I, F)(_.toFloat) ("I2F")
  val I2D = UnaryOp(I, D)(_.toDouble)("I2D")

  val L2I = UnaryOp(J, I)(_.toInt)   ("L2I")
  val L2F = UnaryOp(J, F)(_.toFloat) ("L2F")
  val L2D = UnaryOp(J, D)(_.toDouble)("L2D")

  val F2I = UnaryOp(F, I)(_.toInt)   ("F2I")
  val F2L = UnaryOp(F, J)(_.toLong)  ("F2L")
  val F2D = UnaryOp(F, D)(_.toDouble)("F2D")

  val D2I = UnaryOp(D, I)(_.toInt)   ("D2I")
  val D2L = UnaryOp(D, F)(_.toLong)  ("D2L")
  val D2F = UnaryOp(D, F)(_.toFloat) ("D2F")

  val I2B = UnaryOp(I, B)(_.toByte)  ("I2B")
  val I2C = UnaryOp(I, C)(_.toChar)  ("I2C")
  val I2S = UnaryOp(I, S)(_.toShort) ("I2S")

  val LCmp = BinOp(J, J, I)(_ compare _)("LCmp")
  val FCmpl = BinOp(F, F, I)(_ compare _)("FCmpl")
  val FCmpg = BinOp(F, F, I)(_ compare _)("FCmpg")
  val DCmpl = BinOp(D, D, I)(_ compare _)("DCmpl")
  val DCmpg = BinOp(D, D, I)(_ compare _)("DCmpG")


  case class UnaryBranch(label: Int)
                        (val pred: Int => Boolean)
                        (name: String)extends OpCode

  val IfEq = UnaryBranch(_: Int)(_ == 0)("IfEq")
  val IfNe = UnaryBranch(_: Int)(_ != 0)("IfNe")
  val IfLt = UnaryBranch(_: Int)(_ < 0) ("IfLt")
  val IfGe = UnaryBranch(_: Int)(_ >= 0)("IfGe")
  val IfGt = UnaryBranch(_: Int)(_ > 0) ("IfGt")
  val IfLe = UnaryBranch(_: Int)(_ <= 0)("IfLe")

  case class BinaryBranch(label: Int)
                         (val pred: (Int, Int) => Boolean)
                         (name: String) extends OpCode

  val IfICmpEq = BinaryBranch(_: Int)(_ == _)("IfICmpEq")
  val IfICmpNe = BinaryBranch(_: Int)(_ != _)("IfICmpNe")
  val IfICmpLt = BinaryBranch(_: Int)(_ < _) ("IfICmpLt")
  val IfICmpGe = BinaryBranch(_: Int)(_ >= _)("IfICmpGe")
  val IfICmpGt = BinaryBranch(_: Int)(_ > _) ("IfICmpGt")
  val IfICmpLe = BinaryBranch(_: Int)(_ <= _)("IfICmpLe")

  val IfACmpEq= BinaryBranch(_: Int)(_ == _) ("IfACmpEq")
  val IfACmpNe= BinaryBranch(_: Int)(_ != _)("IfACmpNe")

  case class Goto(label: Int) extends OpCode

  // These guys are meant to be deprecated in java 6 and 7
  //===============================================================
  val Ret = UnusedOpCode
  val Jsr = UnusedOpCode
  //===============================================================

  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends OpCode

  case class LookupSwitch(defaultTarget: Int, keys: Seq[Int], targets: Seq[Int]) extends OpCode
  case class ReturnVal(n: Int) extends OpCode

  val IReturn = ReturnVal(1)
  val LReturn = ReturnVal(2)
  val FReturn = ReturnVal(1)
  val DReturn = ReturnVal(2)
  val AReturn = ReturnVal(1)
  val Return = ReturnVal(0)

  case class GetStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode
  case class PutStatic(owner: Type.Cls, name: String, desc: Type) extends OpCode

  case class GetField(owner: Type.Cls, name: String, desc: Type) extends OpCode
  case class PutField(owner: Type.Cls, name: String, desc: Type) extends OpCode

  case class InvokeVirtual(owner: Type.Ref, sig: imm.Sig) extends OpCode



  case class InvokeSpecial(owner: Type.Cls, sig: imm.Sig) extends OpCode
  case class InvokeStatic(owner: Type.Cls, sig: imm.Sig) extends OpCode
  case class InvokeInterface(owner: Type.Cls, sig: imm.Sig) extends OpCode
  case class InvokeDynamic(name: String, desc: String, bsm: Object, args: Object) extends OpCode

  case class New(desc: Type.Cls) extends OpCode

  case class NewArray(typeCode: Int) extends OpCode
  case class ANewArray(desc: imm.Type.Ref) extends OpCode

  case object ArrayLength extends OpCode
  case object AThrow extends OpCode
  case class CheckCast(desc: Type) extends OpCode

  case class InstanceOf(desc: Type) extends OpCode
  case object MonitorEnter extends OpCode
  case object MonitorExit extends OpCode

  // Not used, because ASM folds these into the following bytecode for us
  //===============================================================
  val Wide = UnusedOpCode
  //===============================================================

  case class MultiANewArray(desc: Type.Arr, dims: Int) extends OpCode
  val IfNull = StackOps.UnaryBranch(_: Int)(_ == 0)("IfNull")
  val IfNonNull = StackOps.UnaryBranch(_: Int)(_ != 0)("IfNull")

  // Not used, because ASM converts these to normal Goto()s and Jsr()s
  //===============================================================
  val GotoW = UnusedOpCode
  val JsrW = UnusedOpCode
  //===============================================================

  def apply(n: Int): Any = all((n + 256) % 256)

  val all = Seq(
    Nop,
    AConstNull,
    IConstM1,
    IConst0,
    IConst1,
    IConst2,
    IConst3,
    IConst4,
    IConst5,
    LConst0,
    LConst1,
    FConst0,
    FConst1,
    FConst2,
    DConst0,
    DConst1,
    BiPush,
    SiPush,
    Ldc,
    LdcW,
    Ldc2W,

    ILoad,
    LLoad,
    FLoad,
    DLoad,
    ALoad,

    ILoad0,
    ILoad1,
    ILoad2,
    ILoad3,

    LLoad0,
    LLoad1,
    LLoad2,
    LLoad3,

    FLoad0,
    FLoad1,
    FLoad2,
    FLoad3,

    DLoad0,
    DLoad1,
    DLoad2,
    DLoad3,

    ALoad0,
    ALoad1,
    ALoad2,
    ALoad3,

    IALoad,
    LALoad,
    FALoad,
    DALoad,
    AALoad,
    BALoad,
    CALoad,
    SALoad,

    IStore,
    LStore,
    FStore,
    DStore,
    AStore,

    IStore0,
    IStore1,
    IStore2,
    IStore3,

    LStore0,
    LStore1,
    LStore2,
    LStore3,

    FStore0,
    FStore1,
    FStore2,
    FStore3,

    DStore0,
    DStore1,
    DStore2,
    DStore3,

    AStore0,
    AStore1,
    AStore2,
    AStore3,

    IAStore,
    LAStore,
    FAStore,
    DAStore,
    AAStore,
    BAStore,
    CAStore,
    SAStore,

    Pop,
    Pop2,
    Dup,
    DupX1,
    DupX2,
    Dup2,
    Dup2X1,
    Dup2X2,
    Swap,

    IAdd,
    LAdd,
    FAdd,
    DAdd,

    ISub,
    LSub,
    FSub,
    DSub,

    IMul,
    LMul,
    FMul,
    DMul,

    IDiv,
    LDiv,
    FDiv,
    DDiv,

    IRem,
    LRem,
    FRem,
    DRem,

    INeg,
    LNeg,
    FNeg,
    DNeg,

    IShl,
    LShl,
    IShr,
    LShr,

    IUShr,
    LUShr,

    IAnd,
    LAnd,

    IOr,
    LOr,

    IXOr,
    LXOr,

    IInc,

    I2L,
    I2F,
    I2D,

    L2I,
    L2F,
    L2D,
    F2I,
    F2L,
    F2D,

    D2I,
    D2L,
    D2F,

    I2B,
    I2C,
    I2S,

    LCmp,
    FCmpl,
    FCmpg,
    DCmpl,
    DCmpg,

    IfEq,
    IfNe,
    IfLt,
    IfGe,
    IfGt,
    IfLe,

    IfICmpEq,
    IfICmpNe,
    IfICmpLt,
    IfICmpGe,
    IfICmpGt,
    IfICmpLe,
    IfACmpEq,
    IfACmpNe,

    Goto,
    Jsr,
    Ret,
    TableSwitch,
    LookupSwitch,
    IReturn,
    LReturn,
    FReturn,
    DReturn,
    AReturn,
    Return,
    GetStatic,
    PutStatic,
    GetField,
    PutField,
    InvokeVirtual,
    InvokeSpecial,
    InvokeStatic,
    InvokeInterface,
    InvokeDynamic,

    New,
    NewArray,
    ANewArray,

    ArrayLength,
    AThrow,
    CheckCast,
    InstanceOf,
    MonitorEnter,
    MonitorExit,
    Wide,
    MultiANewArray,
    IfNull,
    IfNonNull,
    GotoW,
    JsrW
  )
  object UnusedOpCode extends OpCode{
    def op(vt: Thread)  = ???
  }
  implicit def intToByte(n: Int) = n.toByte

}
