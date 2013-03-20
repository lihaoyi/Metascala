package sm

import imm.Type.CharClass
import reflect.ClassTag

/**
 * This vrt contains virtual representations of the data within the java
 * virtual machine, as well as implicit conversions back and forth. Even for
 * data for which the wrappers are simple boxes (e.g. for primitive types) this
 * allows us to keep the "real" data types (which subclass Any) neatly
 * separated from the virtual data types (which subclass vrt.Val).
 *
 * This lets us use implicit conversions to neatly enforce that the proper
 * conversions are being done whenever data is transferred from the outside
 * world into the virtual world.
 */
package object vrt {
  import scala._
  private[this] val unsafe = {
    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    field.get(null).asInstanceOf[sun.misc.Unsafe]
  }

  implicit def virtBoolean(i: Boolean)  = vrt.Boolean(i)
  implicit def virtByte(i: Byte)        = vrt.Byte(i)
  implicit def virtChar(i: Char)        = vrt.Char(i)
  implicit def virtShort(i: Short)      = vrt.Short(i)
  implicit def virtInt(i: Int)          = vrt.Int(i)
  implicit def virtFloat(i: Float)      = vrt.Float(i)
  implicit def virtLong(i: Long)        = vrt.Long(i)
  implicit def virtDouble(i: Double)    = vrt.Double(i)
  implicit def virtNull(i: Null)        = vrt.Null
  implicit def virtUnit(i: Unit)        = vrt.Unit

  def virtualize(i: Any)(implicit vm: VM): vrt.Val = {

    i match{
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Float => x
      case x: Long => x
      case x: Double => x
      case x: Array[Boolean] => new vrt.Arr.Prim(x.clone)
      case x: Array[Byte] => new vrt.Arr.Prim(x.clone)
      case x: Array[Char] => new vrt.Arr.Prim(x.clone)
      case x: Array[Short] => new vrt.Arr.Prim(x.clone)
      case x: Array[Int] => new vrt.Arr.Prim(x.clone)
      case x: Array[Float] => new vrt.Arr.Prim(x.clone)
      case x: Array[Long] => new vrt.Arr.Prim(x.clone)
      case x: Array[Double] => new vrt.Arr.Prim(x.clone)
      new vrt.Arr.Obj(
        imm.Type.read(x.getClass.getComponentType.getName).cast[imm.Type.Entity],
        x.map(x => virtualize(x))
      )
      case x: Any =>

        vrt.Obj(x.getClass.getName.replace('.', '/'),
          x.getClass.getDeclaredFields
            .filter(f => !java.lang.reflect.Modifier.isStatic(f.getModifiers))
            .map{f =>
            f.setAccessible(true)
            f.getName -> virtualize(f.get(x))
          }.toSeq: _*
        )
      case null => vrt.Null
    }
  }


  implicit def virtBooleanArray(i: Array[vrt.Boolean]) = new Arr.Prim(i.map(_.v))
  implicit def virtByteArray(i: Array[vrt.Byte])       = new Arr.Prim(i.map(_.v))
  implicit def virtCharArray(i: Array[vrt.Char])       = new Arr.Prim(i.map(_.v))
  implicit def virtShortArray(i: Array[vrt.Short])     = new Arr.Prim(i.map(_.v))
  implicit def virtIntArray(i: Array[vrt.Int])         = new Arr.Prim(i.map(_.v))
  implicit def virtFloatArray(i: Array[vrt.Float])     = new Arr.Prim(i.map(_.v))
  implicit def virtLongArray(i: Array[vrt.Long])       = new Arr.Prim(i.map(_.v))
  implicit def virtDoubleArray(i: Array[vrt.Double])   = new Arr.Prim(i.map(_.v))
  implicit def virtObjArray[T <% Val](i: Array[T])      = new Arr.Obj(imm.Type.Cls(i.getClass.getComponentType.getName), i.map(x => x: Val))

  def forName(s: String) =
    CharClass.all.find(_.name == s).map(_.realCls).getOrElse[Class[_]](Class.forName(s))


  implicit def unvirtBoolean(i: vrt.Boolean) = i.v
  implicit def unvirtByte(i: vrt.Byte) = i.v
  implicit def unvirtChar(i: vrt.Char) = i.v
  implicit def unvirtShort(i: vrt.Short) = i.v
  implicit def unvirtInt(i: vrt.Int) = i.v
  implicit def unvirtFloat(i: vrt.Float) = i.v
  implicit def unvirtLong(i: vrt.Long) = i.v
  implicit def unvirtDouble(i: vrt.Double) = i.v
  implicit def unvirtNull(i: vrt.Null.type) = null
  implicit def unvirtUnit(i: vrt.Unit.type) = ()

  def unvirtualize(i: vrt.Val): Any = {

    i match{
      case x: vrt.Boolean => x: Boolean
      case x: vrt.Byte => x: Byte
      case x: vrt.Char => x: Char
      case x: vrt.Short => x: Short
      case x: vrt.Int => x: Int
      case x: vrt.Float => x: Float
      case x: vrt.Long => x: Long
      case x: vrt.Double => x: Double
      case x: vrt.Arr.Obj => x.backing.cast[Array[vrt.Val]].map(unvirtualize)
      case vrt.Arr.Prim(backing) => backing.clone()
      case x: vrt.Obj =>
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


  implicit def unvirtBooleanArray(i: vrt.Arr.Prim[Boolean]) = i.backing.clone()
  implicit def unvirtByteArray(i: vrt.Arr.Prim[Byte]) = i.backing.clone()
  implicit def unvirtCharArray(i: vrt.Arr.Prim[Char]) = i.backing.clone()
  implicit def unvirtShortArray(i: vrt.Arr.Prim[Short]) = i.backing.clone()
  implicit def unvirtIntArray(i: vrt.Arr.Prim[Int]) = i.backing.clone()
  implicit def unvirtFloatArray(i: vrt.Arr.Prim[Float]) = i.backing.clone()
  implicit def unvirtLongArray(i: vrt.Arr.Prim[Long]) = i.backing.clone()
  implicit def unvirtDoubleArray(i: vrt.Arr.Prim[Double]) = i.backing.clone()

  implicit def unvirtArray[T](i: vrt.Arr.Obj)(implicit ct: ClassTag[T], f: Val => T) = {
    i.backing.map(x => x: T)
  }

  implicit def virtString(i: String)(implicit vm: VM): vrt.Obj = {
    vrt.Obj("java/lang/String",
      "value" -> new vrt.Arr.Prim(i.toCharArray)
    )
  }
  implicit def unvirtString(i: vrt.Obj) = {
    new String(i.members(0)("value").cast[vrt.Arr].backing.map{case x: Char => x})
  }
}

