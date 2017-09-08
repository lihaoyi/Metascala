package metascala.features

import java.awt.geom.Point2D

import metascala.TestUtil._
import metascala.{Gen, VM, Virtualizer}
import metascala.Gen._
import utest._


object ReflectTests extends utest.TestSuite {
  def sortNames[T <: java.lang.reflect.Member](in: Array[T]) = {
    in.map(_.getName).sorted.toList
  }
  def sortModifiers[T <: java.lang.reflect.Member](in: Array[T]) = {
    in.map(x => x.getName -> x.getModifiers)
      .sorted
      .toList
  }

  implicit val intAll10 = 10 ** Gen.intAll
  def tests = this {
    val tester = new VM()
    "getSetX" - {

      "obj" - tester.testFunc { () =>
        val string = new String("i am a cow")
        val f = classOf[String].getDeclaredField("value")
        f.setAccessible(true)
        f.set(string, Array('o', 'm', 'g'))
        f.get(string)
      }
      "bool" - tester.testFunc { () =>
        val bool = new java.lang.Boolean(false)
        val f = classOf[java.lang.Boolean].getDeclaredField("value")
        f.setAccessible(true)
        f.setBoolean(bool, true)
        f.getBoolean(bool)
      }
      "byte" - tester.testFunc { () =>
        val byte = new java.lang.Byte(123.toByte)
        val f = classOf[java.lang.Byte].getDeclaredField("value")
        f.setAccessible(true)
        f.setByte(byte, 314.toByte)
        f.getByte(byte)
      }
      "char" - tester.testFunc { () =>
        val char = new java.lang.Character('o')
        val f = classOf[java.lang.Character].getDeclaredField("value")
        f.setAccessible(true)
        f.setChar(char, '1')
        f.getChar(char)
      }
      "int" - tester.testFunc { () =>
        val string = new String("i am a cow")
        string.hashCode
        val f = classOf[String].getDeclaredField("hash")
        f.setAccessible(true)
        f.setInt(string, 123456789)
        f.getInt(string)
      }
      "float" - tester.testFunc { () =>
        val point = new Point2D.Float(1.337f, 2.7182f)
        val f = classOf[Point2D.Float].getDeclaredField("x")
        f.setAccessible(true)
        f.setFloat(point, 123123123f)
        f.getFloat(point)
      }
      "long" - tester.testFunc { () =>
        val long = new java.lang.Long(10)
        val f = classOf[java.lang.Long].getDeclaredField("value")
        f.setAccessible(true)
        f.setLong(long, 1234567890l)
        f.getLong(long)
      }
      "double" - tester.testFunc { () =>
        val point = new Point2D.Double(31.337, 27.182)
        val f = classOf[Point2D.Double].getDeclaredField("x")
        f.setAccessible(true)
        f.setDouble(point, 3133.7)
        f.getDouble(point)
      }
    }
    "memberListing" - {

      "getName" - tester.test {
        classOf[java.util.Properties].getName
      }
      "getCanonicalName" - tester.test {
        classOf[java.util.Properties].getCanonicalName
      }


      "getFieldsNames" - tester.test {
        sortNames(classOf[String].getFields)
      }
      "getFieldsNames2" - tester.test {
        sortNames(classOf[java.util.Properties].getFields)
      }
      "getDeclaredFieldsNames" - tester.test {
        sortNames(classOf[String].getDeclaredFields)
      }
      "getDeclaredFieldsNames2" - tester.test {
        sortNames(classOf[java.util.Properties].getDeclaredFields)
      }


      "getMethodsNames" - tester.test {
        sortNames(classOf[String].getMethods)
      }
      "getMethodsNames2" - tester.test {
        sortNames(classOf[java.util.Properties].getMethods)
      }
      "getDeclaredMethodsNames" - tester.test {
        sortNames(classOf[String].getDeclaredMethods)
      }
      "getDeclaredMethodsNames2" - tester.test {
        sortNames(classOf[java.util.Properties].getDeclaredMethods)
      }

      "getConstructors" - tester.test {
        sortNames(classOf[String].getConstructors)
      }
      "getConstructors2" - tester.test {
        sortNames(classOf[java.util.Properties].getConstructors)
      }

      "getDeclaredConstructors" - tester.test {
        sortNames(classOf[String].getDeclaredConstructors)
      }
      "getDeclaredConstructors2" - tester.test {
        sortNames(classOf[java.util.Properties].getDeclaredConstructors)
      }


      "getDeclaredFieldsModifiers" - tester.test {
        sortModifiers(classOf[String].getDeclaredFields)
      }
      "getDeclaredFieldsModifiers2" - tester.test {
        sortModifiers(classOf[java.util.Properties].getDeclaredFields)
      }


      "getDeclaredMethodsModifiers" - tester.test {
        sortModifiers(classOf[String].getDeclaredMethods)
      }
      "getDeclaredMethodsModifiers2" - tester.test {
        sortModifiers(classOf[java.util.Properties].getDeclaredMethods)
      }

      "getDeclaredConstructorModifiers" - tester.test {
        sortModifiers(classOf[String].getDeclaredConstructors)
      }
      "getDeclaredConstructorModifiers2" - tester.test {
        sortModifiers(classOf[java.util.Properties].getDeclaredConstructors)
      }
    }

    //  "getSetStatic" - {
    //    "double" - tester.testFunc{ () =>
    //      val f = classOf[Point2D.Double].getDeclaredField("serialVersionUID")
    //      f.setAccessible(true)
    //      f.getDouble(null)
    //    }
    //  }
    "allocate" - {
      val p = Virtualizer.unsafe
        .allocateInstance(classOf[Point2D.Float])
        .asInstanceOf[Point2D.Float]
      p.x = 10
      p.y = 20
      p.x * p.x + p.y * p.y
    }


  }
}

