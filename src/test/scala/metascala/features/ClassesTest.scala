package metascala
package features

import utest._
import metascala.TestUtil

object ClassesTest extends utest.TestSuite {
  import TestUtil._

  def tests = this {
    "classes" - {
      val tester = new VM()
      "customClass" - tester.test {
        val c1 = new Cow()
        val c2 = new Cow()
        c1.moo.length() + c2.moo.length()
      }
      "stringConcat" - tester.test {
        val x = new StringBuilder()
        x.append('a')
        x.toString()
      }
      "inheritance" - tester.test {
        val b = new Bull
        b.mooTwice
      }
      "constructor" - tester.test {
        val m = new Matrix(5, 7, -1, 3)
        m.determinant
      }
      "superConstructor" - tester.test {
        val m = new DoubleMatrix(2, 4, -8, 4)
        m.determinant
      }
      "override" - tester.test {
        val m = new DoubleDetMatrix(1, 2, 3, 4)
        m.determinant
      }
      "innerClass" - tester.test {
        val l: LinkedList = new LinkedList
        var i: Int = 0
        while (i < 2) {
          l.push(i)
          i += 1
        }

        l.sum
      }
    }

    "inheritance" - {
      val tester = new Tester("metascala.features.classes.Inheritance")
      "implement" - tester.run("implement", 10)
      "abstractClass" - tester.run("abstractClass")
      "shadowedInheritedGet" - tester.run("shadowedInheritedGet")
      "shadowedInheritedSet" - tester.run("shadowedInheritedSet")
      "superMethod" - tester.run("superMethod")
      "staticInheritance" - tester.run("staticInheritance")
      "staticInheritanceMethod" - tester.run("staticInheritanceMethod")
      "interfaceDefault" - tester.run("interfaceDefault", 19)
      "indirectInterfaceDefault" - tester.run("indirectInterfaceDefault", 19)
      "overridenDefault" - tester.run("overridenDefault", 19)
    }
  }
}

class Cow {
  def moo: String = {
    return "moooo"
  }
}

class Bull extends Cow {
  def mooTwice: String = {
    return moo + moo
  }
}

class Matrix(aa: Float, ab: Float, ba: Float, bb: Float) {
  def determinant: Float = {
    return aa * bb - ab * ba
  }
}

class DoubleMatrix(aa: Float, ab: Float, ba: Float, bb: Float)
  extends Matrix(aa*2, ab*2, ba*2, bb*2)

class DoubleDetMatrix(aa: Float, ab: Float, ba: Float, bb: Float)
  extends Matrix(aa*2, ab*2, ba*2, bb*2){

  override def determinant: Float = {
    return super.determinant * 2
  }
}

class LinkedList {
  def push(i: Int) {
    val n = new Inner(i, head)
    head = n
  }

  def sum: Int = {
    var curr: Inner = head
    var total: Int = 0
    while (curr != null) {
      total = total + head.value
      curr = curr.next
    }
    return total
  }

  var head: Inner = null

  class Inner(val value: Int, val next: Inner)
}

