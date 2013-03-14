package sm

import reflect.ClassTag


package object virt {

  val unsafe = {
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
  def virtualize(i: Any)(implicit vm: VM): virt.Val = i match{
    case x: scala.Boolean => x
    case x: scala.Byte => x
    case x: scala.Char => x
    case x: scala.Short => x
    case x: scala.Int => x
    case x: scala.Float => x
    case x: scala.Long => x
    case x: scala.Double => x
    case x: Array[_] => virt.Arr(imm.Type.read(x.getClass.getComponentType.getName).cast[imm.Type.Entity], x.map(x => virtualize(x)))
    case x: AnyRef =>

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
  implicit def virtBoolean(i: scala.Boolean) = Boolean(i)
  implicit def virtByte(i: scala.Byte) = Byte(i)
  implicit def virtChar(i: scala.Char) = Char(i)
  implicit def virtShort(i: scala.Short) = Short(i)
  implicit def virtInt(i: scala.Int) = Int(i)
  implicit def virtFloat(i: scala.Float) = Float(i)
  implicit def virtLong(i: scala.Long) = Long(i)
  implicit def virtDouble(i: scala.Double) = Double(i)
  implicit def virtNull(i: scala.Null) = Null
  implicit def virtUnit(i: scala.Unit) = Unit
  implicit def virtArray[T <% Val](i: Array[T]) = new Arr(imm.Type.Cls("java/lang/Object"), i.map(x => x: Val))

  def forName(s: String) =
    primitiveMap.get(s).getOrElse[Class[_]](Class.forName(s))

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
      case x: virt.Arr =>
        println("Unvirt Array " + x.tpe.unparse)
        val newArr = java.lang.reflect.Array.newInstance(forName(x.tpe.name.replace('/', '.')), x.backing.length)
        type SBoolean = scala.Boolean
        type SByte = scala.Byte
        type SChar = scala.Char
        type SShort = scala.Short
        type SInt = scala.Int
        type SFloat = scala.Float
        type SLong = scala.Long
        type SDouble = scala.Double
        newArr match{
          case a: Array[SBoolean] => x.backing.map(_.cast[virt.Byte].v != 0).copyToArray(a)
          case a: Array[SByte] => x.backing.map(_.cast[virt.Byte].v).copyToArray(a)
          case a: Array[SChar] => x.backing.map(_.cast[virt.Char].v).copyToArray(a)
          case a: Array[SShort] => x.backing.map(_.cast[virt.Short].v).copyToArray(a)
          case a: Array[SInt] => x.backing.map(_.cast[virt.Int].v).copyToArray(a)
          case a: Array[SFloat] => x.backing.map(_.cast[virt.Float].v).copyToArray(a)
          case a: Array[SLong] => x.backing.map(_.cast[virt.Long].v).copyToArray(a)
          case a: Array[SDouble] => x.backing.map(_.cast[virt.Double].v).copyToArray(a)
          case a: Array[Any] => x.backing.map(unvirtualize).copyToArray(a)
        }
        println(x.backing.map(unvirtualize).toSeq.map(_.getClass))

        //System.arraycopy(x.backing.map(unvirtualize), 0, newArr, 0, x.backing.length)
        newArr
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
  implicit def unvirtArray[T](i: Arr)(implicit ct: ClassTag[T], f: Val => T) = {
    i.backing.map(x => x: T)
  }

  implicit def virtString(i: String)(implicit vm: VM) = {
    virt.Obj("java/lang/String",
      "value" -> virt.Arr(imm.Type.Prim("C"), i.toCharArray.map(x => virtChar(x) : Val))
    )
  }
  implicit def unvirtString(i: virt.Obj) = {
    new String(i.members(0)("value").cast[virt.Arr].backing.map{case x: virt.Char => x.v})
  }
}

