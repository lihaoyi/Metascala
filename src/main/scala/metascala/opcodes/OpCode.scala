package metascala
package opcodes

import metascala.{VM}
import collection.mutable
import org.objectweb.asm
import asm.Label
import org.objectweb.asm.tree._
import collection.convert.wrapAsScala._
import rt.Thread


abstract class OpCode{
  def op(vt: Thread): Any
}

object OpCode {
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




  def apply(n: Int): Any = all((n + 256) % 256)
  import Misc._
  import LoadStore._
  import StackManip._

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




