package metascala

import imm.Type.Prim.Info
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
  lazy val unsafe = {
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
  implicit def virtUnit(i: Unit)(implicit vm: VM)        = virtualize(i)

  def virtualize(i: Any)(implicit vm: VM): vrt.Val = {
    i match{
      case null => vrt.Null
      case x: Boolean => x
      case x: Byte =>    x
      case x: Char =>    x
      case x: Short =>   x
      case x: Int =>     x
      case x: Float =>   x
      case x: Long =>    x
      case x: Double =>  x
      case x: Array[Boolean] => vrt.Arr.Prim(x.clone)
      case x: Array[Byte]    => vrt.Arr.Prim(x.clone)
      case x: Array[Char]    => vrt.Arr.Prim(x.clone)
      case x: Array[Short]   => vrt.Arr.Prim(x.clone)
      case x: Array[Int]     => vrt.Arr.Prim(x.clone)
      case x: Array[Float]   => vrt.Arr.Prim(x.clone)
      case x: Array[Long]    => vrt.Arr.Prim(x.clone)
      case x: Array[Double]  => vrt.Arr.Prim(x.clone)
      case x: Array[Any] =>
        vrt.Arr.Obj(
          imm.Type.read(x.getClass.getComponentType.getName.replace('.', '/')).cast[imm.Type.Ref],
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

    }
  }


  implicit def virtBooleanArray(i: Array[scala.Boolean])(implicit vm: VM) = Arr.Prim(i)
  implicit def virtByteArray(i: Array[scala.Byte])(implicit vm: VM)       = Arr.Prim(i)
  implicit def virtCharArray(i: Array[scala.Char])(implicit vm: VM)       = Arr.Prim(i)
  implicit def virtShortArray(i: Array[scala.Short])(implicit vm: VM)     = Arr.Prim(i)
  implicit def virtIntArray(i: Array[scala.Int])(implicit vm: VM)         = Arr.Prim(i)
  implicit def virtFloatArray(i: Array[scala.Float])(implicit vm: VM)     = Arr.Prim(i)
  implicit def virtLongArray(i: Array[scala.Long])(implicit vm: VM)       = Arr.Prim(i)
  implicit def virtDoubleArray(i: Array[scala.Double])(implicit vm: VM)   = Arr.Prim(i)
  implicit def virtObjArray[T <: vrt.Obj with vrt.Ref](i: Array[T])(implicit vm: VM)      = Arr.Obj(imm.Type.Cls(i.getClass.getComponentType.getName.replace('.', '/')).asInstanceOf[imm.Type.Ref], i.map(x => x: Val))


  def forName(s: String) =
    imm.Type.Prim.Info.all.find(_.name == s).map(_.realCls).getOrElse[Class[_]](Class.forName(s))


  implicit def unvirtBoolean(i: vrt.Boolean) = i.v
  implicit def unvirtByte(i: vrt.Byte) = i.v
  implicit def unvirtChar(i: vrt.Char) = i.v
  implicit def unvirtShort(i: vrt.Short) = i.v
  implicit def unvirtInt(i: vrt.Int) = i.v
  implicit def unvirtFloat(i: vrt.Float) = i.v
  implicit def unvirtLong(i: vrt.Long) = i.v
  implicit def unvirtDouble(i: vrt.Double) = i.v
  implicit def unvirtNull(i: vrt.Null.type) = null

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
      case x: vrt.Arr.Obj => x.view.cast[Array[vrt.Val]].map(unvirtualize)
      case vrt.Arr.Prim(backing) => backing.clone()
      case x: vrt.Obj =>
        val cls = Class.forName(x.cls.name.replace('/', '.'))
        val obj = unsafe.allocateInstance(cls)

        obj
      case vrt.Null => null
    }
  }


  implicit def unvirtBooleanArray(i: vrt.Arr.Prim[Boolean]) = i.view.clone()
  implicit def unvirtByteArray(i: vrt.Arr.Prim[Byte]) = i.view.clone()
  implicit def unvirtCharArray(i: vrt.Arr.Prim[Char]) = i.view.clone()
  implicit def unvirtShortArray(i: vrt.Arr.Prim[Short]) = i.view.clone()
  implicit def unvirtIntArray(i: vrt.Arr.Prim[Int]) = i.view.clone()
  implicit def unvirtFloatArray(i: vrt.Arr.Prim[Float]) = i.view.clone()
  implicit def unvirtLongArray(i: vrt.Arr.Prim[Long]) = i.view.clone()
  implicit def unvirtDoubleArray(i: vrt.Arr.Prim[Double]) = i.view.clone()

  implicit def unvirtArray[T](i: vrt.Arr.Obj)(implicit ct: ClassTag[T], f: Val => T) = {
    i.view
  }

  implicit def virtString(i: String)(implicit vm: VM): vrt.Obj = {
    vrt.Obj("java/lang/String",
      "value" -> vrt.Arr.Prim(i.toCharArray)
    )
  }
  implicit def unvirtString(i: vrt.Obj) = {
    new String(i(imm.Type.Cls("java/lang/String"), "value").cast[vrt.Arr[Char]].view)
  }
}

