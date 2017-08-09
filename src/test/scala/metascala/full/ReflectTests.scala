package metascala
package full

import org.scalatest.FreeSpec

import metascala.Gen._
import java.awt.geom.Point2D


class ReflectTests extends FreeSpec {
  implicit val intAll10 = 10 ** Gen.intAll
  import Util._
  val tester = new VM()
  "getSetX" - {

    "obj" in tester.testFunc{ () =>
      val string = new String("i am a cow")
      val f = classOf[String].getDeclaredField("value")
      f.setAccessible(true)
      f.set(string, Array('o', 'm', 'g'))
      f.get(string)
    }
    "bool" in tester.testFunc{ () =>
      val bool = new java.lang.Boolean(false)
      val f = classOf[java.lang.Boolean].getDeclaredField("value")
      f.setAccessible(true)
      f.setBoolean(bool, true)
      f.getBoolean(bool)
    }
    "byte" in tester.testFunc{ () =>
      val byte = new java.lang.Byte(123.toByte)
      val f = classOf[java.lang.Byte].getDeclaredField("value")
      f.setAccessible(true)
      f.setByte(byte, 314.toByte)
      f.getByte(byte)
    }
    "char" in tester.testFunc{ () =>
      val char = new java.lang.Character('o')
      val f = classOf[java.lang.Character].getDeclaredField("value")
      f.setAccessible(true)
      f.setChar(char, '1')
      f.getChar(char)
    }
    "int" in tester.testFunc{ () =>
      val string = new String("i am a cow")
      string.hashCode
      val f = classOf[String].getDeclaredField("hash")
      f.setAccessible(true)
      f.setInt(string, 123456789)
      f.getInt(string)
    }
    "float" in tester.testFunc{ () =>
      val point = new Point2D.Float(1.337f, 2.7182f)
      val f = classOf[Point2D.Float].getDeclaredField("x")
      f.setAccessible(true)
      f.setFloat(point, 123123123f)
      f.getFloat(point)
    }
    "long" in tester.testFunc{ () =>
      val long = new java.lang.Long(10)
      val f = classOf[java.lang.Long].getDeclaredField("value")
      f.setAccessible(true)
      f.setLong(long, 1234567890l)
      f.getLong(long)
    }
    "double" in tester.testFunc{ () =>
      val point = new Point2D.Double(31.337, 27.182)
      val f = classOf[Point2D.Double].getDeclaredField("x")
      f.setAccessible(true)
      f.setDouble(point, 3133.7)
      f.getDouble(point)
    }
  }
  "getSetStatic" - {
//    "double" in tester.testFunc{ () =>
//      val f = classOf[Point2D.Double].getDeclaredField("serialVersionUID")
//      f.setAccessible(true)
//      f.getDouble(null)
//    }
  }
  "allocate" in {
    val p = Virtualizer.unsafe
                       .allocateInstance(classOf[Point2D.Float])
                       .asInstanceOf[Point2D.Float]
    p.x = 10
    p.y = 20
    p.x * p.x + p.y * p.y
  }

}

