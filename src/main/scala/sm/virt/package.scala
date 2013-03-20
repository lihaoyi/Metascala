package sm

import imm.Type.CharClass
import reflect.ClassTag

/**
 * This package contains virtual representations of the data within the java
 * virtual machine, as well as implicit conversions back and forth. Even for
 * data for which the wrappers are simple boxes (e.g. for primitive types) this
 * allows us to keep the "real" data types (which subclass Any) neatly
 * separated from the virtual data types (which subclass virt.Val).
 *
 * This lets us use implicit conversions to neatly enforce that the proper
 * conversions are being done whenever data is transferred from the outside
 * world into the virtual world.
 */
package object virt {
  import scala._
  private[this] val unsafe = {
    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    field.get(null).asInstanceOf[sun.misc.Unsafe]
  }

  implicit def virtBoolean(i: Boolean)  = virt.Boolean(i)
  implicit def virtByte(i: Byte)        = virt.Byte(i)
  implicit def virtChar(i: Char)        = virt.Char(i)
  implicit def virtShort(i: Short)      = virt.Short(i)
  implicit def virtInt(i: Int)          = virt.Int(i)
  implicit def virtFloat(i: Float)      = virt.Float(i)
  implicit def virtLong(i: Long)        = virt.Long(i)
  implicit def virtDouble(i: Double)    = virt.Double(i)
  implicit def virtNull(i: Null)        = virt.Null
  implicit def virtUnit(i: Unit)        = virt.Unit

  def virtualize(i: Any)(implicit vm: VM): virt.Val = {

    i match{
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Float => x
      case x: Long => x
      case x: Double => x
      case x: Array[Boolean] => new virt.PrimArr(x.clone)
      case x: Array[Byte] => new virt.PrimArr(x.clone)
      case x: Array[Char] => new virt.PrimArr(x.clone)
      case x: Array[Short] => new virt.PrimArr(x.clone)
      case x: Array[Int] => new virt.PrimArr(x.clone)
      case x: Array[Float] => new virt.PrimArr(x.clone)
      case x: Array[Long] => new virt.PrimArr(x.clone)
      case x: Array[Double] => new virt.PrimArr(x.clone)
      new virt.ObjArr(
        imm.Type.read(x.getClass.getComponentType.getName).cast[imm.Type.Entity],
        x.map(x => virtualize(x))
      )
      case x: Any =>

        virt.Obj(x.getClass.getName.replace('.', '/'),
          x.getClass.getDeclaredFields
            .filter(f => !java.lang.reflect.Modifier.isStatic(f.getModifiers))
            .map{f =>
            f.setAccessible(true)
            f.getName -> virtualize(f.get(x))
          }.toSeq: _*
        )
      case null => virt.Null
    }
  }


  implicit def virtBooleanArray(i: Array[virt.Boolean]) = new PrimArr(i.map(_.v))
  implicit def virtByteArray(i: Array[virt.Byte])       = new PrimArr(i.map(_.v))
  implicit def virtCharArray(i: Array[virt.Char])       = new PrimArr(i.map(_.v))
  implicit def virtShortArray(i: Array[virt.Short])     = new PrimArr(i.map(_.v))
  implicit def virtIntArray(i: Array[virt.Int])         = new PrimArr(i.map(_.v))
  implicit def virtFloatArray(i: Array[virt.Float])     = new PrimArr(i.map(_.v))
  implicit def virtLongArray(i: Array[virt.Long])       = new PrimArr(i.map(_.v))
  implicit def virtDoubleArray(i: Array[virt.Double])   = new PrimArr(i.map(_.v))
  implicit def virtObjArray[T <% Val](i: Array[T])      = new ObjArr(imm.Type.Cls(i.getClass.getComponentType.getName), i.map(x => x: Val))

  def forName(s: String) =
    CharClass.all.find(_.name == s).map(_.realCls).getOrElse[Class[_]](Class.forName(s))


  implicit def unvirtBoolean(i: virt.Boolean) = i.v
  implicit def unvirtByte(i: virt.Byte) = i.v
  implicit def unvirtChar(i: virt.Char) = i.v
  implicit def unvirtShort(i: virt.Short) = i.v
  implicit def unvirtInt(i: virt.Int) = i.v
  implicit def unvirtFloat(i: virt.Float) = i.v
  implicit def unvirtLong(i: virt.Long) = i.v
  implicit def unvirtDouble(i: virt.Double) = i.v
  implicit def unvirtNull(i: virt.Null.type) = null
  implicit def unvirtUnit(i: virt.Unit.type) = ()

  def unvirtualize(i: virt.Val): Any = {

    i match{
      case x: virt.Boolean => x: Boolean
      case x: virt.Byte => x: Byte
      case x: virt.Char => x: Char
      case x: virt.Short => x: Short
      case x: virt.Int => x: Int
      case x: virt.Float => x: Float
      case x: virt.Long => x: Long
      case x: virt.Double => x: Double
      case x: virt.ObjArr => x.backing.cast[Array[virt.Val]].map(unvirtualize)
      case virt.PrimArr(backing) => backing.clone()
      case x: virt.Obj =>
        val cls = Class.forName(x.cls.name.replace('/', '.'))
        val obj = unsafe.allocateInstance(cls)
        x.members(0).foreach{ case (k, v) =>
          val field = cls.getDeclaredField(k)
          field.setAccessible(true)
          field.set(obj, unvirtualize(v))
        }
        obj
      case Null => null
    }
  }


  implicit def unvirtBooleanArray(i: virt.PrimArr[Boolean]) = i.backing.clone()
  implicit def unvirtByteArray(i: virt.PrimArr[Byte]) = i.backing.clone()
  implicit def unvirtCharArray(i: virt.PrimArr[Char]) = i.backing.clone()
  implicit def unvirtShortArray(i: virt.PrimArr[Short]) = i.backing.clone()
  implicit def unvirtIntArray(i: virt.PrimArr[Int]) = i.backing.clone()
  implicit def unvirtFloatArray(i: virt.PrimArr[Float]) = i.backing.clone()
  implicit def unvirtLongArray(i: virt.PrimArr[Long]) = i.backing.clone()
  implicit def unvirtDoubleArray(i: virt.PrimArr[Double]) = i.backing.clone()

  implicit def unvirtArray[T](i: virt.ObjArr)(implicit ct: ClassTag[T], f: Val => T) = {
    i.backing.map(x => x: T)
  }

  implicit def virtString(i: String)(implicit vm: VM): virt.Obj = {
    virt.Obj("java/lang/String",
      "value" -> new virt.PrimArr(i.toCharArray)
    )
  }
  implicit def unvirtString(i: virt.Obj) = {
    new String(i.members(0)("value").cast[virt.Arr].backing.map{case x: Char => x})
  }
}

