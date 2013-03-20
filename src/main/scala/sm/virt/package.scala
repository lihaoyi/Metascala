package sm

import imm.Type.CharClass
import reflect.ClassTag

/**
 * This package contains virtual representations of the data within the java
 * virtual machine, as well as implicit conversions back and forth. Even for
 * data for which the wrappers are simple boxes (e.g. for primitive types) this
 * allows us to keep the "real" data types (which subclass scala.Any) neatly
 * separated from the virtual data types (which subclass virt.Val).
 *
 * This lets us use implicit conversions to neatly enforce that the proper
 * conversions are being done whenever data is transferred from the outside
 * world into the virtual world.
 */
package object virt {

  private[this] val unsafe = {
    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    field.get(null).asInstanceOf[sun.misc.Unsafe]
  }
  val primitiveMap = Map[String, Class[_]](

    "int" -> classOf[scala.Int],
    "long" -> classOf[scala.Long],
    "double" -> classOf[scala.Double],
    "float" -> classOf[scala.Float],
    "boolean" -> classOf[scala.Boolean],
    "char" -> classOf[scala.Char],
    "byte" -> classOf[scala.Byte],
    "short" -> classOf[scala.Short]

  )
  implicit def virtBoolean(i: scala.Boolean)  = Boolean(i)
  implicit def virtByte(i: scala.Byte)        = Byte(i)
  implicit def virtChar(i: scala.Char)        = Char(i)
  implicit def virtShort(i: scala.Short)      = Short(i)
  implicit def virtInt(i: scala.Int)          = Int(i)
  implicit def virtFloat(i: scala.Float)      = Float(i)
  implicit def virtLong(i: scala.Long)        = Long(i)
  implicit def virtDouble(i: scala.Double)    = Double(i)
  implicit def virtNull(i: scala.Null)        = Null
  implicit def virtUnit(i: scala.Unit)        = Unit

  def virtualize(i: Any)(implicit vm: VM): virt.Val = {
    type SBoolean = scala.Boolean
    type SByte = scala.Byte
    type SChar = scala.Char
    type SShort = scala.Short
    type SInt = scala.Int
    type SFloat = scala.Float
    type SLong = scala.Long
    type SDouble = scala.Double
    i match{
      case x: scala.Boolean => x
      case x: scala.Byte => x
      case x: scala.Char => x
      case x: scala.Short => x
      case x: scala.Int => x
      case x: scala.Float => x
      case x: scala.Long => x
      case x: scala.Double => x
      case x: scala.Array[SBoolean] => new virt.PrimArr[scala.Boolean](x.clone)
      case x: scala.Array[SByte] => new virt.PrimArr[scala.Byte](x.clone)
      case x: scala.Array[SChar] => new virt.PrimArr[scala.Char](x.clone)
      case x: scala.Array[SShort] => new virt.PrimArr[scala.Short](x.clone)
      case x: scala.Array[SInt] => new virt.PrimArr[scala.Int](x.clone)
      case x: scala.Array[SFloat] => new virt.PrimArr[scala.Float](x.clone)
      case x: scala.Array[SLong] => new virt.PrimArr[scala.Long](x.clone)
      case x: scala.Array[SDouble] => new virt.PrimArr[scala.Double](x.clone)
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
    primitiveMap.get(s).getOrElse[Class[_]](Class.forName(s))


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
      case x: virt.Boolean => x: scala.Boolean
      case x: virt.Byte => x: scala.Byte
      case x: virt.Char => x: scala.Char
      case x: virt.Short => x: scala.Short
      case x: virt.Int => x: scala.Int
      case x: virt.Float => x: scala.Float
      case x: virt.Long => x: scala.Long
      case x: virt.Double => x: scala.Double
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


  implicit def unvirtBooleanArray(i: virt.PrimArr[scala.Boolean]) = i.backing.clone()
  implicit def unvirtByteArray(i: virt.PrimArr[scala.Byte]) = i.backing.clone()
  implicit def unvirtCharArray(i: virt.PrimArr[scala.Char]) = i.backing.clone()
  implicit def unvirtShortArray(i: virt.PrimArr[scala.Short]) = i.backing.clone()
  implicit def unvirtIntArray(i: virt.PrimArr[scala.Int]) = i.backing.clone()
  implicit def unvirtFloatArray(i: virt.PrimArr[scala.Float]) = i.backing.clone()
  implicit def unvirtLongArray(i: virt.PrimArr[scala.Long]) = i.backing.clone()
  implicit def unvirtDoubleArray(i: virt.PrimArr[scala.Double]) = i.backing.clone()

  implicit def unvirtArray[T](i: virt.ObjArr)(implicit ct: ClassTag[T], f: Val => T) = {
    i.backing.map(x => x: T)
  }

  implicit def virtString(i: String)(implicit vm: VM): virt.Obj = {
    virt.Obj("java/lang/String",
      "value" -> new virt.PrimArr(i.toCharArray)
    )
  }
  implicit def unvirtString(i: virt.Obj) = {
    new String(i.members(0)("value").cast[virt.Arr].backing.map{case x: scala.Char => x})
  }
}

