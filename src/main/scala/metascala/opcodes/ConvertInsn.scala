package metascala
package opcodes



import org.objectweb.asm.Type
import scala.collection.mutable
import metascala.imm.Type.Prim
import metascala.imm.Type.Prim._
import scala.annotation.tailrec
import org.objectweb.asm.tree._
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.Opcodes._

import Insn._
object ConvertInsn {
  case class F1[A, B](a: A => B, override val toString: String) extends Function1[A, B]{
    def apply(x: A) = a(x)
  }
  case class F2[A, B, C](a: (A, B) => C, override val toString: String) extends Function2[A, B, C]{
    def apply(x: A, y: B) = a(x, y)
  }
  def apply(insn: AbstractInsnNode,
              append: Insn => Unit,
              frame: Frame[Box],
              nextFrame: Frame[Box],
              blockMap: Array[Int])
             (implicit vm: VM, getBox: Box => Int, deref: LabelNode => Int) = {

    def push[T](v: T, tpe: Prim[T]) = {
      append(Push(nextFrame.top(), tpe, v))
    }
    def aLoad[T](p: Prim[T], tpe: imm.Type) ={
      append(GetArray(nextFrame.top(), frame.top(), frame.top(1), tpe))
    }
    def aStore[T](p: Prim[T], tpe: imm.Type) ={
      append(PutArray(frame.top(), frame.top(1), frame.top(2), tpe))
    }
    def binOp1[A](a: Prim[A])(f: (A, A) => A) = binOp(a, a, a)(f)
    def binOp[A, B, C](a: Prim[A], b: Prim[B], c: Prim[C])(f: (A, B) => C) = {
      val (bb, aa) = (getBox(frame.top()), getBox(frame.top(1)))
      append(BinOp(aa, a, bb, b, nextFrame.top(), c, f))
    }
    def unaryOp[A, B](a: Prim[A], b: Prim[B])(f: A => B) = {
      append(UnaryOp(frame.top(), a, nextFrame.top(), b, f))
    }
    def unaryBranch(label: LabelNode, f: Int => Boolean) = {
      append(Insn.UnaryBranch(frame.top(), label, f))
    }
    def binaryBranch(label: LabelNode, f: (Int, Int) => Boolean) = {
      append(Insn.BinaryBranch(frame.top(), frame.top(1), label, f))
    }
    def returnVal(tpe: imm.Type) = {
      append(Insn.ReturnVal(if (tpe == V) 0 else frame.top()))
    }
    def invokeVirtual(insn: MethodInsnNode, indexed: Boolean) = {
      val desc = imm.Desc.read(insn.desc)

      val cls = imm.Type.read(insn.owner) match{
        case c: imm.Type.Cls => c
        case _ => imm.Type.Cls("java/lang/Object")
      }

      val args = for(j <- (0 until desc.args.length + 1).reverse) yield {
        frame.top(j)
      }
      getBox(args.last)
      val sig = new imm.Sig(insn.name, desc)
      val target = if (desc.ret == V) 0 else nextFrame.top(): Int

      val mIndex =
        if (!indexed) -1
        else vm.ClsTable(cls).vTable.indexWhere(_.sig == sig)

      append(Insn.InvokeVirtual(target, args.map(getBox), cls.cast[imm.Type.Cls], sig, mIndex))
    }
    def invokeStatic(insn: MethodInsnNode, self: Int) = {
      val desc = imm.Desc.read(insn.desc)
      val m = vm.resolveDirectRef(insn.owner, imm.Sig(insn.name, desc)).get

      val args = for(j <- (0 until desc.args.length + self).reverse) yield {
        frame.top(j)
      }

      val target = if (desc.ret == V) 0 else nextFrame.top(): Int

      append(Insn.InvokeStatic(target, args.map(getBox), insn.owner, m))
    }

    implicit def intInsnNode(x: AbstractInsnNode) = x.cast[IntInsnNode]
    implicit def jumpInsnNode(x: AbstractInsnNode) = x.cast[JumpInsnNode]
    implicit def methodInsnNode(x: AbstractInsnNode) = {
      x match {
        case x: MethodInsnNode => x
        case x: FieldInsnNode => new MethodInsnNode(x.getOpcode, x.owner, x.name, x.desc)
      }
    }
    def refField(list: rt.Cls => Seq[imm.Field], func: (rt.Cls, Int, imm.Type) => Insn) = {

      def resolve(cls: rt.Cls): (Int, rt.Cls) = {
        list(cls).lastIndexWhere(_.name == insn.name) match{
          case -1 => resolve(cls.clsAncestry(1).cls)
          case x =>  (x, cls)
        }
      }
      val (index, cls) = resolve(insn.owner.cls)
      assert(
        index >= 0,
        s"no field found in ${insn.owner}: ${insn.name}\n" +
        "Fields\n" + list(insn.owner.cls).map(_.name).mkString("\n")
      )
      val prim = list(cls)(index).desc
      append(func(cls, index - prim.size + 1, prim))
    }

    insn.getOpcode match{
      case ICONST_M1 => push(-1, I)
      case ICONST_0 => push(0, I)
      case ICONST_1 => push(1, I)
      case ICONST_2 => push(2, I)
      case ICONST_3 => push(3, I)
      case ICONST_4 => push(4, I)
      case ICONST_5 => push(5, I)
      case LCONST_0 => push(0L, J)
      case LCONST_1 => push(1L, J)
      case FCONST_0 => push(0F, F)
      case FCONST_1 => push(1F, F)
      case FCONST_2 => push(2F, F)
      case DCONST_0 => push(0D, D)
      case DCONST_1 => push(1D, D)
      case BIPUSH => push(insn.operand, I)
      case SIPUSH => push(insn.operand, I)
      case LDC =>
        insn.cast[LdcInsnNode].cst match{
          case s: String =>
            val top = vm.alloc(Virtualizer.pushVirtual(s)(_)).apply(0)
            val index = vm.interned.length
            vm.interned.append(new ManualRef(top))
            append(Ldc(nextFrame.top(), index))
          case t: org.objectweb.asm.Type =>
            val clsObj = vm.typeObjCache(imm.Type.read(t.getInternalName))
            val index = vm.interned.length
            vm.interned.append(new ManualRef(clsObj()))
            append(Ldc(nextFrame.top(), index))
          case x: java.lang.Byte  => append(Insn.Push(nextFrame.top(), B, x: Byte))
          case x: java.lang.Character  => append(Insn.Push(nextFrame.top(), C, x: Char))
          case x: java.lang.Short => append(Insn.Push(nextFrame.top(), S, x: Short))
          case x: java.lang.Integer   => append(Insn.Push(nextFrame.top(), I, x: Int))
          case x: java.lang.Float => append(Insn.Push(nextFrame.top(), F, x: Float))
          case x: java.lang.Long  => append(Insn.Push(nextFrame.top(), J, x: Long))
          case x: java.lang.Double => append(Insn.Push(nextFrame.top(), D, x: Double))
        }

      case IALOAD => aLoad(I, I)
      case LALOAD => aLoad(J, J)
      case FALOAD => aLoad(F, F)
      case DALOAD => aLoad(D, D)
      case AALOAD => aLoad(I, imm.Type.Cls("java/lang/Object"))
      case BALOAD => aLoad(B, B)
      case CALOAD => aLoad(C, C)
      case SALOAD => aLoad(S, S)
      case IASTORE => aStore(I, I)
      case LASTORE => aStore(J, J)
      case FASTORE => aStore(F, F)
      case DASTORE => aStore(D, D)
      case AASTORE => aStore(I, imm.Type.Cls("java/lang/Object"))
      case BASTORE => aStore(B, B)
      case CASTORE => aStore(C, C)
      case SASTORE => aStore(S, S)
      case IADD => binOp1(I)(F2(_ + _, "IAdd"))
      case LADD => binOp1(J)(F2(_ + _, "LAdd"))
      case FADD => binOp1(F)(F2(_ + _, "FAdd"))
      case DADD => binOp1(D)(F2(_ + _, "DAdd"))
      case ISUB => binOp1(I)(F2(_ - _, "ISub"))
      case LSUB => binOp1(J)(F2(_ - _, "LSub"))
      case FSUB => binOp1(F)(F2(_ - _, "FSub"))
      case DSUB => binOp1(D)(F2(_ - _, "DSub"))
      case IMUL => binOp1(I)(F2(_ * _, "IMul"))
      case LMUL => binOp1(J)(F2(_ * _, "LMul"))
      case FMUL => binOp1(F)(F2(_ * _, "FMul"))
      case DMUL => binOp1(D)(F2(_ * _, "DMul"))
      case IDIV => binOp1(I)(F2(_ / _, "IDiv"))
      case LDIV => binOp1(J)(F2(_ / _, "LDiv"))
      case FDIV => binOp1(F)(F2(_ / _, "FDiv"))
      case DDIV => binOp1(D)(F2(_ / _, "DDiv"))
      case IREM => binOp1(I)(F2(_ % _, "IRem"))
      case LREM => binOp1(J)(F2(_ % _, "LRem"))
      case FREM => binOp1(F)(F2(_ % _, "FRem"))
      case DREM => binOp1(D)(F2(_ % _, "DRem"))
      case INEG => unaryOp(I, I)(F1(-_, "INeg"))
      case LNEG => unaryOp(J, J)(F1(-_, "LNeg"))
      case FNEG => unaryOp(F, F)(F1(-_, "FNeg"))
      case DNEG => unaryOp(D, D)(F1(-_, "DNeg"))
      case ISHL => binOp(I, I, I)(F2(_ << _, "IShl"))
      case LSHL => binOp(J, I, J)(F2(_ << _, "LShl"))
      case ISHR => binOp(I, I, I)(F2(_ >> _, "IShr"))
      case LSHR => binOp(J, I, J)(F2(_ >> _, "LShr"))
      case IUSHR => binOp(I, I, I)(F2(_ >>> _, "IUShr"))
      case LUSHR => binOp(J, I, J)(F2(_ >>> _, "LUShr"))
      case IAND => binOp(I, I, I)(F2(_ & _, "IAnd"))
      case LAND => binOp(J, J, J)(F2(_ & _, "LAnd"))
      case IOR => binOp(I, I, I)(F2(_ | _, "IOr"))
      case LOR => binOp(J, J, J)(F2(_ | _, "LOr"))
      case IXOR => binOp(I, I, I)(F2(_ ^ _, "IXOr"))
      case LXOR => binOp(J, J, J)(F2(_ ^ _, "LXOr"))
      case IINC =>
        val x = insn.cast[IincInsnNode]
        val bvalue = new Box(new BasicValue(org.objectweb.asm.Type.INT_TYPE))
        append(Insn.Push(bvalue, I, x.incr))
        append(Insn.BinOp[I, I, I](bvalue, I, frame.getLocal(x.`var`), I, nextFrame.getLocal(x.`var`), I, F2[Int, Int, Int](_+_, "IInc")))
      case I2L => unaryOp(I, J)(F1(_.toLong,  "I2L"))
      case I2F => unaryOp(I, F)(F1(_.toFloat, "I2F"))
      case I2D => unaryOp(I, D)(F1(_.toDouble,"I2D"))
      case L2I => unaryOp(J, I)(F1(_.toInt,   "L2I"))
      case L2F => unaryOp(J, F)(F1(_.toFloat, "L2F"))
      case L2D => unaryOp(J, D)(F1(_.toDouble,"L2D"))
      case F2I => unaryOp(F, I)(F1(_.toInt,   "F2I"))
      case F2L => unaryOp(F, J)(F1(_.toLong,  "F2L"))
      case F2D => unaryOp(F, D)(F1(_.toDouble,"F2D"))
      case D2I => unaryOp(D, I)(F1(_.toInt,   "D2I"))
      case D2L => unaryOp(D, F)(F1(_.toLong,  "D2L"))
      case D2F => unaryOp(D, F)(F1(_.toFloat, "D2F"))
      case I2B => unaryOp(I, B)(F1(_.toByte,  "I2B"))
      case I2C => unaryOp(I, C)(F1(_.toChar,  "I2C"))
      case I2S => unaryOp(I, S)(F1(_.toShort, "I2S"))
      case LCMP => binOp(J, J, I)(F2(_ compare _, "LCmp"))
      case FCMPL => binOp(F, F, I)(F2(_ compare _, "FCmpl"))
      case FCMPG => binOp(F, F, I)(F2(_ compare _, "FCmpg"))
      case DCMPL => binOp(D, D, I)(F2(_ compare _, "DCmpl"))
      case DCMPG => binOp(D, D, I)(F2(_ compare _, "DCmpG"))
      case IFEQ => unaryBranch(insn.label, F1(_ == 0, "IfEq"))
      case IFNE => unaryBranch(insn.label, F1(_ != 0, "IfNe"))
      case IFLT => unaryBranch(insn.label, F1(_ < 0,  "IfLt"))
      case IFGE => unaryBranch(insn.label, F1(_ >= 0, "IfGe"))
      case IFGT => unaryBranch(insn.label, F1(_ > 0,  "IfGt"))
      case IFLE => unaryBranch(insn.label, F1(_ <= 0, "IfLe"))
      case IF_ICMPEQ => binaryBranch(insn.label, F2(_ == _, "IfICmpEq"))
      case IF_ICMPNE => binaryBranch(insn.label, F2(_ != _, "IfICmpNe"))
      case IF_ICMPLT => binaryBranch(insn.label, F2(_ < _,  "IfICmpLt"))
      case IF_ICMPGE => binaryBranch(insn.label, F2(_ >= _, "IfICmpGe"))
      case IF_ICMPGT => binaryBranch(insn.label, F2(_ > _,  "IfICmpGt"))
      case IF_ICMPLE => binaryBranch(insn.label, F2(_ <= _, "IfICmpLe"))
      case IF_ACMPEQ => binaryBranch(insn.label, F2(_ == _, "IfACmpEq"))
      case IF_ACMPNE => binaryBranch(insn.label, F2(_ != _, "IfACmpNe"))
      case GOTO => append(Insn.Goto(insn.label))
      case TABLESWITCH   =>
        val x = insn.cast[TableSwitchInsnNode]
        append(Insn.TableSwitch(frame.top(), x.min, x.max, x.dflt, x.labels.map(deref)))
      case LOOKUPSWITCH =>
        val x = insn.cast[LookupSwitchInsnNode]
        append(Insn.LookupSwitch(frame.top(), x.dflt, x.keys.asScala.map(_.intValue), x.labels.map(deref)))
      case IRETURN => returnVal(I)
      case LRETURN => returnVal(J)
      case FRETURN => returnVal(F)
      case DRETURN => returnVal(D)
      case ARETURN => returnVal(imm.Type.Cls("java/lang/Object"))
      case RETURN => returnVal(V)
      case GETSTATIC => refField(_.staticList, Insn.GetStatic(nextFrame.top(), _, _, _))
      case PUTSTATIC => refField(_.staticList, Insn.PutStatic(frame.top(), _, _, _))
      case GETFIELD  => refField(_.fieldList, (a, b, c) => Insn.GetField(nextFrame.top(), frame.top(), b, c))
      case PUTFIELD  => refField(_.fieldList, (a, b, c) => Insn.PutField(frame.top(), frame.top(1), b, c))
      case INVOKESTATIC    => invokeStatic(insn, 0)
      case INVOKESPECIAL   => invokeStatic(insn, 1)
      case INVOKEVIRTUAL   => invokeVirtual(insn, indexed=true)
      case INVOKEINTERFACE => invokeVirtual(insn, indexed=false)
      case NEW => append(Insn.New(nextFrame.top(), vm.ClsTable(insn.cast[TypeInsnNode].desc)))
      case NEWARRAY =>
        val typeRef: imm.Type = insn.operand match{
          case 4  => Z: imm.Type
          case 5  => C: imm.Type
          case 6  => F: imm.Type
          case 7  => D: imm.Type
          case 8  => B: imm.Type
          case 9  => S: imm.Type
          case 10 => I: imm.Type
          case 11 => J: imm.Type
        }
        append(Insn.NewArray(frame.top(), nextFrame.top(), typeRef))
      case ANEWARRAY   => append(Insn.NewArray(frame.top(), nextFrame.top(), imm.Type.read(insn.cast[TypeInsnNode].desc)))
      case ARRAYLENGTH => append(Insn.ArrayLength(frame.top(), nextFrame.top()))
      case ATHROW      => append(Insn.AThrow(frame.top()))
      case CHECKCAST   => append(Insn.CheckCast(frame.top(), nextFrame.top(), imm.Type.read(insn.cast[TypeInsnNode].desc)))
      case INSTANCEOF  => append(Insn.InstanceOf(frame.top(), nextFrame.top(), imm.Type.read(insn.cast[TypeInsnNode].desc)))
      case MULTIANEWARRAY =>
        val x = insn.cast[MultiANewArrayInsnNode]
        val dims = (0 until x.dims).reverse.map(frame.top)
        append(Insn.MultiANewArray(imm.Type.read(x.desc), nextFrame.top(), dims.map(getBox)))
      case IFNULL    => unaryBranch(insn.label, F1(_ == 0, "IfNull"))
      case IFNONNULL => unaryBranch(insn.label, F1(_ != 0, "IfNonNull"))
      case ACONST_NULL | POP | POP2 | DUP | DUP_X1 | DUP_X2 | DUP2 |
           DUP2_X1 | DUP2_X2 | SWAP | ISTORE | LSTORE | FSTORE | DSTORE |
           ASTORE | ILOAD | LLOAD | FLOAD | DLOAD | ALOAD | NOP | -1 =>
        () // These are "move" operations and can be ignored
      case MONITORENTER | MONITOREXIT => () // No monitors!
    }
  }
}
