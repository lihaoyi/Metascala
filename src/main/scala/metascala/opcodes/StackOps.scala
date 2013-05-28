package metascala

import StackOps._
import rt.Thread
import org.objectweb.asm.Label
import org.objectweb.asm.tree._
import collection.convert.wrapAsScala._

import imm.Type
import imm.Type.Prim
import imm.Type.Prim._

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
  object UnusedOpCode extends OpCode{
    def op(vt: Thread)  = ???
  }
  implicit def intToByte(n: Int) = n.toByte
  trait Jump
  case class F1[A, B](a: A => B, override val toString: String) extends Function1[A, B]{
    def apply(x: A) = a(x)
  }
  case class F2[A, B, C](a: (A, B) => C, override val toString: String) extends Function2[A, B, C]{
    def apply(x: A, y: B) = a(x, y)
  }

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

  case class Const[A](b: Prim[A], value: A) extends OpCode{
    def words = {
      var out = List[Int]()
      b.write(value, out ::= _)
      out
    }
  }

  val AConstNull = Const[I](I, 0)
  val IConstM1 = Const[I](I, -1)

  val IConst0 = Const[I](I, 0)
  val IConst1 = Const[I](I, 1)
  val IConst2 = Const[I](I, 2)
  val IConst3 = Const[I](I, 3)
  val IConst4 = Const[I](I, 4)
  val IConst5 = Const[I](I, 5)

  val LConst0 = Const[J](J, 0)
  val LConst1 = Const[J](J, 1)

  val FConst0 = Const[F](F, 0)
  val FConst1 = Const[F](F, 1)
  val FConst2 = Const[F](F, 2)

  val DConst0 = Const[D](D, 0)
  val DConst1 = Const[D](D, 1)

  case class Push(value: Int) extends OpCode
  val BiPush = Push(_: Int)
  val SiPush = Push(_: Int)


  case class Ldc(const: Any) extends OpCode

  // Not used, because ASM converts these Ldc(const: Any)
  //===============================================================
  val LdcW = UnusedOpCode
  val Ldc2W = UnusedOpCode
  //===============================================================


  case class Load[T](index: Int, p: Prim[T]) extends OpCode

  val ILoad = Load(_: Int, I)
  val LLoad = Load(_: Int, J)
  val FLoad = Load(_: Int, F)
  val DLoad = Load(_: Int, D)
  val ALoad = Load(_: Int, I)



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

  case class LoadArray[T](p: Prim[T]) extends OpCode
  val IALoad = LoadArray(I)
  val LALoad = LoadArray(J)
  val FALoad = LoadArray(F)
  val DALoad = LoadArray(D)
  val AALoad = LoadArray(I)
  val BALoad = LoadArray(B)
  val CALoad = LoadArray(C)
  val SALoad = LoadArray(S)

  case class Store[T](index: Int, p: Prim[T]) extends OpCode

  val IStore = Store[I](_: Int, I)
  val LStore = Store[J](_: Int, J)
  val FStore = Store[F](_: Int, F)
  val DStore = Store[D](_: Int, D)
  val AStore = Store[I](_: Int, I)

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


  case class StoreArray[T](p: Prim[T]) extends OpCode
  val IAStore = StoreArray(I)
  val LAStore = StoreArray(J)
  val FAStore = StoreArray(F)
  val DAStore = StoreArray(D)
  val AAStore = StoreArray(I)
  val BAStore = StoreArray(B)
  val CAStore = StoreArray(C)
  val SAStore = StoreArray(S)

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


  val IAdd = BinOp[I, I, I](I, I, I, F2(_ + _, "IAdd"))
  val LAdd = BinOp[J, J, J](J, J, J, F2(_ + _, "LAdd"))
  val FAdd = BinOp[F, F, F](F, F, F, F2(_ + _, "FAdd"))
  val DAdd = BinOp[D, D, D](D, D, D, F2(_ + _, "DAdd"))

  val ISub = BinOp[I, I, I](I, I, I, F2(_ - _, "ISub"))
  val LSub = BinOp[J, J, J](J, J, J, F2(_ - _, "LSub"))
  val FSub = BinOp[F, F, F](F, F, F, F2(_ - _, "FSub"))
  val DSub = BinOp[D, D, D](D, D, D, F2(_ - _, "DSub"))

  val IMul = BinOp[I, I, I](I, I, I, F2(_ * _, "IMul"))
  val LMul = BinOp[J, J, J](J, J, J, F2(_ * _, "LMul"))
  val FMul = BinOp[F, F, F](F, F, F, F2(_ * _, "FMul"))
  val DMul = BinOp[D, D, D](D, D, D, F2(_ * _, "DMul"))

  val IDiv = BinOp[I, I, I](I, I, I, F2(_ / _, "IDiv"))
  val LDiv = BinOp[J, J, J](J, J, J, F2(_ / _, "LDiv"))
  val FDiv = BinOp[F, F, F](F, F, F, F2(_ / _, "FDiv"))
  val DDiv = BinOp[D, D, D](D, D, D, F2(_ / _, "DDiv"))

  val IRem = BinOp[I, I, I](I, I, I, F2(_ % _, "IRem"))
  val LRem = BinOp[J, J, J](J, J, J, F2(_ % _, "LRem"))
  val FRem = BinOp[F, F, F](F, F, F, F2(_ % _, "FRem"))
  val DRem = BinOp[D, D, D](D, D, D, F2(_ % _, "DRem"))

  val INeg = UnaryOp[I, I](I, I, F1(-_, "INeg"))
  val LNeg = UnaryOp[J, J](J, J, F1(-_, "LNeg"))
  val FNeg = UnaryOp[F, F](F, F, F1(-_, "FNeg"))
  val DNeg = UnaryOp[D, D](D, D, F1(-_, "DNeg"))

  val IShl = BinOp[I, I, I](I, I, I, F2(_ << _, "IShl"))
  val LShl = BinOp[I, J, J](I, J, J, F2(_ << _, "LShl"))
  val IShr = BinOp[I, I, I](I, I, I, F2(_ >> _, "IShr"))
  val LShr = BinOp[I, J, J](I, J, J, F2(_ >> _, "LShr"))

  val IUShr = BinOp[I, I, I](I, I, I, F2(_ >>> _, "IUShr"))
  val LUShr = BinOp[I, J, J](I, J, J, F2(_ >>> _, "LUShr"))

  val IAnd = BinOp[I, I, I](I, I, I, F2(_ & _, "IAnd"))
  val LAnd = BinOp[J, J, J](J, J, J, F2(_ & _, "LAnd"))

  val IOr = BinOp[I, I, I](I, I, I, F2(_ | _, "IOr"))
  val LOr = BinOp[J, J, J](J, J, J, F2(_ | _, "LOr"))

  val IXOr = BinOp[I, I, I](I, I, I, F2(_ ^ _, "IXOr"))
  val LXOr = BinOp[J, J, J](J, J, J, F2(_ ^ _, "LXOr"))

  case class IInc(varId: Int, amount: Int) extends OpCode

  case class UnaryOp[A, R](a: Prim[A], out: Prim[R], func: A => R) extends OpCode

  case class BinOp[A, B, R](a: Prim[A], b: Prim[B], out: Prim[R], func: (B, A) => R) extends OpCode

  val I2L = UnaryOp[I, J](I, J, F1(_.toLong,  "I2L"))
  val I2F = UnaryOp[I, F](I, F, F1(_.toFloat, "I2F"))
  val I2D = UnaryOp[I, D](I, D, F1(_.toDouble,"I2D"))

  val L2I = UnaryOp[J, I](J, I, F1(_.toInt,   "L2I"))
  val L2F = UnaryOp[J, F](J, F, F1(_.toFloat, "L2F"))
  val L2D = UnaryOp[J, D](J, D, F1(_.toDouble,"L2D"))

  val F2I = UnaryOp[F, I](F, I, F1(_.toInt,   "F2I"))
  val F2L = UnaryOp[F, J](F, J, F1(_.toLong,  "F2L"))
  val F2D = UnaryOp[F, D](F, D, F1(_.toDouble,"F2D"))

  val D2I = UnaryOp[D, I](D, I, F1(_.toInt,   "D2I"))
  val D2L = UnaryOp[D, F](D, F, F1(_.toLong,  "D2L"))
  val D2F = UnaryOp[D, F](D, F, F1(_.toFloat, "D2F"))

  val I2B = UnaryOp[I, B](I, B, F1(_.toByte,  "I2B"))
  val I2C = UnaryOp[I, C](I, C, F1(_.toChar,  "I2C"))
  val I2S = UnaryOp[I, S](I, S, F1(_.toShort, "I2S"))

  val LCmp =  BinOp[J, J, I](J, J, I, F2(_ compare _, "LCmp"))
  val FCmpl = BinOp[F, F, I](F, F, I, F2(_ compare _, "FCmpl"))
  val FCmpg = BinOp[F, F, I](F, F, I, F2(_ compare _, "FCmpg"))
  val DCmpl = BinOp[D, D, I](D, D, I, F2(_ compare _, "DCmpl"))
  val DCmpg = BinOp[D, D, I](D, D, I, F2(_ compare _, "DCmpG"))


  case class UnaryBranch(label: Int, pred: Int => Boolean) extends OpCode with Jump

  val IfEq = UnaryBranch(_: Int, F1(_ == 0, "IfEq"))
  val IfNe = UnaryBranch(_: Int, F1(_ != 0, "IfNe"))
  val IfLt = UnaryBranch(_: Int, F1(_ < 0,  "IfLt"))
  val IfGe = UnaryBranch(_: Int, F1(_ >= 0, "IfGe"))
  val IfGt = UnaryBranch(_: Int, F1(_ > 0,  "IfGt"))
  val IfLe = UnaryBranch(_: Int, F1(_ <= 0, "IfLe"))

  case class BinaryBranch(label: Int, pred: (Int, Int) => Boolean) extends OpCode with Jump

  val IfICmpEq = BinaryBranch(_: Int, F2(_ == _, "IfICmpEq"))
  val IfICmpNe = BinaryBranch(_: Int, F2(_ != _, "IfICmpNe"))
  val IfICmpLt = BinaryBranch(_: Int, F2(_ < _,  "IfICmpLt"))
  val IfICmpGe = BinaryBranch(_: Int, F2(_ >= _, "IfICmpGe"))
  val IfICmpGt = BinaryBranch(_: Int, F2(_ > _,  "IfICmpGt"))
  val IfICmpLe = BinaryBranch(_: Int, F2(_ <= _, "IfICmpLe"))

  val IfACmpEq= BinaryBranch(_: Int, F2(_ == _, "IfACmpEq"))
  val IfACmpNe= BinaryBranch(_: Int, F2(_ != _, "IfACmpNe"))

  case class Goto(label: Int) extends OpCode with Jump

  // These guys are meant to be deprecated in java 6 and 7
  //===============================================================
  val Ret = UnusedOpCode
  val Jsr = UnusedOpCode
  //===============================================================

  case class TableSwitch(min: Int, max: Int, defaultTarget: Int, targets: Seq[Int]) extends OpCode with Jump

  case class LookupSwitch(defaultTarget: Int, keys: Seq[Int], targets: Seq[Int]) extends OpCode with Jump
  case class ReturnVal(n: Int) extends OpCode with Jump

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
  val IfNull    = StackOps.UnaryBranch(_: Int, F1(_ == 0, "IfNull"))
  val IfNonNull = StackOps.UnaryBranch(_: Int, F1(_ != 0, "IfNonNull"))

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





}
