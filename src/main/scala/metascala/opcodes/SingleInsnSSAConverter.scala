package metascala
package opcodes


import scala.collection.mutable
import metascala.imm.Type.Prim
import metascala.imm.Type.Prim._
import org.objectweb.asm.tree._

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import org.objectweb.asm.tree.analysis._
import org.objectweb.asm.Opcodes._
import Insn._
import metascala.rt.Logger
import metascala.util.{Agg, Ref, WritableRef}
object SingleInsnSSAConverter {
  trait VMInterface extends rt.Obj.VMInterface{
    def alloc[T](func: rt.Allocator => T): T
    def arr(address: Int): rt.Arr
    def resolveDirectRef(owner: imm.Type.Cls, sig: imm.Sig): Option[rt.Method]
    val interned: mutable.Buffer[WritableRef]
    val typeObjCache: mutable.HashMap[imm.Type, WritableRef]
    def logger: Logger
  }


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
            deref: LabelNode => Int,
            box: Box => Int)
           (implicit vm: VMInterface) = {
    def top(x: Frame[Box], n: Int = 0) = box(x.getStack(x.getStackSize - 1 - n))
    def push[T](v: T, tpe: Prim[T]) = {
      append(Push(top(nextFrame), tpe, v))
    }
    def aLoad[T](p: Prim[T], tpe: imm.Type) ={
      append(GetArray(top(nextFrame), top(frame), top(frame, 1), tpe))
    }
    def aStore[T](p: Prim[T], tpe: imm.Type) ={
      append(PutArray(top(frame), top(frame, 1), top(frame, 2), tpe))
    }
    def binOp1[A](a: Prim[A])(f: (A, A) => A) = binOp(a, a, a)(f)
    def binOp[A, B, C](a: Prim[A], b: Prim[B], c: Prim[C])(f: (A, B) => C) = {
      val (bb, aa) = (top(frame), top(frame, 1))
      append(BinOp(aa, a, bb, b, top(nextFrame), c, f))
    }
    def unaryOp[A, B](a: Prim[A], b: Prim[B])(f: A => B) = {
      append(UnaryOp(top(frame), a, top(nextFrame), b, f))
    }
    def unaryBranch(label: LabelNode, f: Int => Boolean) = {
      append(Insn.UnaryBranch(top(frame), deref(label), f))
    }
    def binaryBranch(label: LabelNode, f: (Int, Int) => Boolean) = {
      append(Insn.BinaryBranch(top(frame), top(frame, 1), deref(label), f))
    }
    def returnVal(tpe: imm.Type) = {
      append(Insn.ReturnVal(if (tpe == V) 0 else top(frame)))
    }
    def invokeVirtual(insn: MethodInsnNode, indexed: Boolean) = {
      val desc = imm.Desc.read(insn.desc)

      val cls = imm.Type.read(insn.owner) match{
        case c: imm.Type.Cls => c
        case _ => imm.Type.Cls("java.lang.Object")
      }

      val args = for(j <- (0 until desc.args.length + 1).reverse) yield {
        top(frame, j)
      }
      val sig = new imm.Sig(insn.name, desc)
      val target = if (desc.ret == V) 0 else top(nextFrame): Int

      val runtimeCls = vm.ClsTable(cls)
      val mIndex =
        if (!indexed) -1
        else runtimeCls.vTable.indexWhere(_.sig == sig)

      val clsIndex = vm.ClsTable.clsIndex.indexOf(runtimeCls)
      append(Insn.InvokeVirtual(target, Agg.from(args), clsIndex, sig, mIndex))
    }

    def invokeStatic(insn: MethodInsnNode, special: Boolean) = {
      val desc = imm.Desc.read(insn.desc)

      val selfArgCount = if(special) 1 else 0
      val args = for(j <- (0 until desc.args.length + selfArgCount).reverse) yield {
        top(frame, j)
      }
      val sig = imm.Sig(insn.name, desc)

      val target = if (desc.ret == V) 0 else top(nextFrame): Int
      val cls = imm.Type.read(insn.owner) match{
        case c: imm.Type.Cls => c
        case _ => imm.Type.Cls("java.lang.Object")
      }
      val runtimeCls = vm.ClsTable(cls)
      val mIndex =
        if (special) runtimeCls.vTable.indexWhere(_.sig == sig)
        else runtimeCls.staticTable.indexWhere(_.sig == sig)

      val clsIndex = vm.ClsTable.clsIndex.indexOf(runtimeCls)
      append(Insn.InvokeStatic(target, Agg.from(args), clsIndex, mIndex, special))
    }
    def invokeDynamic(insn: InvokeDynamicInsnNode) = {
      append(Insn.InvokeDynamic(
        insn.name,
        insn.desc,
        insn.bsm.getTag,
        insn.bsm.getOwner,
        insn.bsm.getName,
        insn.bsm.getDesc,
        insn.bsmArgs
      ))
    }
    implicit def intInsnNode(x: AbstractInsnNode) = x.asInstanceOf[IntInsnNode]
    implicit def jumpInsnNode(x: AbstractInsnNode) = x.asInstanceOf[JumpInsnNode]
    implicit def methodInsnNode(x: AbstractInsnNode) = {
      x match {
        case x: MethodInsnNode => x
        case x: FieldInsnNode => new MethodInsnNode(x.getOpcode, x.owner, x.name, x.desc)
      }
    }
    def refField(list: rt.Cls => Seq[imm.Field], func: (Int, Int, imm.Type) => Insn) = {

      def resolve(cls: rt.Cls): (Int, rt.Cls) = {
        list(cls).lastIndexWhere(_.name == insn.name) match{
          case -1 => resolve(vm.ClsTable(cls.clsAncestry(1)))
          case x =>  (x, cls)
        }
      }
      val (index, cls) = resolve(vm.ClsTable(insn.owner))
      assert(
        index >= 0,
        s"no field found in ${insn.owner}: ${insn.name}\n" +
        "Fields\n" + list(vm.ClsTable(insn.owner)).map(_.name).mkString("\n")
      )
      val prim = list(cls)(index).desc
      append(func(vm.ClsTable.clsIndex.indexOf(cls), index - prim.size + 1, prim))
    }
    def refStaticField(list: rt.Cls => Seq[imm.Field], func: (Int, Int, imm.Type) => Insn) = {
      // Unlike instance fields, and unlike static methods, static fields can
      // be inherited from both the main superclass as well as from any auxilliary
      // interfaces you implement!
      val seen = mutable.Set.empty[imm.Type.Cls]
      def resolve(cls: rt.Cls): Seq[(Int, rt.Cls)] = {
        if (seen(cls.tpe)) Nil
        else {
          seen.add(cls.tpe)
          cls.staticList.lastIndexWhere(_.name == insn.name) match{
            case -1 =>
              (cls.superType.toSeq ++ cls.interfaces).flatMap(ancestor => resolve(vm.ClsTable(ancestor)))
            case n => Seq((n, cls))
          }
        }
      }
      resolve(vm.ClsTable(insn.owner)) match{
        case Seq((index, cls)) =>
          assert(index >= 0, s"no field found in ${insn.owner}: ${insn.name}\n")
          val prim = cls.staticList(index).desc
          append(func(vm.ClsTable.clsIndex.indexOf(cls), index - prim.size + 1, prim))
        case res => throw new Exception("Zero or Multiple Possible Fields Inherited! " + res)
      }
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
        insn.asInstanceOf[LdcInsnNode].cst match{
          case s: String =>
            val index = vm.interned.length
            vm.interned.append(new Ref.UnsafeManual(vm.alloc(Virtualizer.pushVirtual(s)(_)).apply(0)))
            append(Ldc(top(nextFrame), index))
          case t: org.objectweb.asm.Type =>
            val clsObj = vm.typeObjCache(imm.Type.read(t.getInternalName))
            val index = vm.interned.length
            vm.interned.append(new Ref.UnsafeManual(clsObj()))
            append(Ldc(top(nextFrame), index))
          case x: java.lang.Byte  => append(Insn.Push(top(nextFrame), B, x: Byte))
          case x: java.lang.Character  => append(Insn.Push(top(nextFrame), C, x: Char))
          case x: java.lang.Short => append(Insn.Push(top(nextFrame), S, x: Short))
          case x: java.lang.Integer   => append(Insn.Push(top(nextFrame), I, x: Int))
          case x: java.lang.Float => append(Insn.Push(top(nextFrame), F, x: Float))
          case x: java.lang.Long  => append(Insn.Push(top(nextFrame), J, x: Long))
          case x: java.lang.Double => append(Insn.Push(top(nextFrame), D, x: Double))
        }

      case IALOAD => aLoad(I, I)
      case LALOAD => aLoad(J, J)
      case FALOAD => aLoad(F, F)
      case DALOAD => aLoad(D, D)
      case AALOAD => aLoad(I, "java.lang.Object")
      case BALOAD => aLoad(B, B)
      case CALOAD => aLoad(C, C)
      case SALOAD => aLoad(S, S)
      case IASTORE => aStore(I, I)
      case LASTORE => aStore(J, J)
      case FASTORE => aStore(F, F)
      case DASTORE => aStore(D, D)
      case AASTORE => aStore(I, "java.lang.Object")
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
        val x = insn.asInstanceOf[IincInsnNode]
        val bvalue = new Box(new BasicValue(org.objectweb.asm.Type.INT_TYPE))
        append(Insn.Push(box(bvalue), I, x.incr))
        append(Insn.BinOp[I, I, I](
          box(bvalue), I,
          box(frame.getLocal(x.`var`)), I,
          box(nextFrame.getLocal(x.`var`)), I,
          F2[Int, Int, Int](_+_, "IInc")
        ))
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
      case D2L => unaryOp(D, J)(F1(_.toLong,  "D2L"))
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
      case GOTO => append(Insn.Goto(deref(insn.label)))
      case TABLESWITCH   =>
        val x = insn.asInstanceOf[TableSwitchInsnNode]
        append(Insn.TableSwitch(top(frame), x.min, x.max, deref(x.dflt), Agg.from(x.labels.map(deref))))
      case LOOKUPSWITCH =>
        val x = insn.asInstanceOf[LookupSwitchInsnNode]
        append(Insn.LookupSwitch(top(frame), deref(x.dflt), Agg.from(x.keys.asScala.map(_.intValue)), Agg.from(x.labels.map(deref))))
      case IRETURN => returnVal(I)
      case LRETURN => returnVal(J)
      case FRETURN => returnVal(F)
      case DRETURN => returnVal(D)
      case ARETURN => returnVal("java.lang.Object")
      case RETURN => returnVal(V)
      case GETSTATIC => refStaticField(_.staticList, Insn.GetStatic(top(nextFrame), _, _, _))
      case PUTSTATIC => refStaticField(_.staticList, Insn.PutStatic(top(frame), _, _, _))
      case GETFIELD  => refField(_.fieldList, (a, b, c) => Insn.GetField(top(nextFrame), top(frame), b, c))
      case PUTFIELD  => refField(_.fieldList, (a, b, c) => Insn.PutField(top(frame), top(frame, 1), b, c))
      case INVOKESTATIC    => invokeStatic(insn, special = false)
      case INVOKESPECIAL   => invokeStatic(insn, special = true)
      case INVOKEVIRTUAL   => invokeVirtual(insn, indexed = true)
      case INVOKEINTERFACE => invokeVirtual(insn, indexed = false)
      case INVOKEDYNAMIC => invokeDynamic(insn.asInstanceOf[InvokeDynamicInsnNode])
      case NEW =>
        append(Insn.New(
          top(nextFrame),
          vm.ClsTable.clsIndex.indexOf(vm.ClsTable(insn.asInstanceOf[TypeInsnNode].desc))
        ))
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
        append(Insn.NewArray(top(frame), top(nextFrame), typeRef))
      case ANEWARRAY   => append(Insn.NewArray(top(frame), top(nextFrame), imm.Type.read(insn.asInstanceOf[TypeInsnNode].desc)))
      case ARRAYLENGTH => append(Insn.ArrayLength(top(frame), top(nextFrame)))
      case ATHROW      => append(Insn.AThrow(top(frame)))
      case CHECKCAST   => append(Insn.CheckCast(top(frame), top(nextFrame), imm.Type.read(insn.asInstanceOf[TypeInsnNode].desc)))
      case INSTANCEOF  => append(Insn.InstanceOf(top(frame), top(nextFrame), imm.Type.read(insn.asInstanceOf[TypeInsnNode].desc)))
      case MULTIANEWARRAY =>
        val x = insn.asInstanceOf[MultiANewArrayInsnNode]
        val dims = (0 until x.dims).reverse.map(top(frame, _))
        append(Insn.MultiANewArray(imm.Type.read(x.desc), top(nextFrame), dims))
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
