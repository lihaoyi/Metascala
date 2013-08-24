package metascala
package features

import org.scalatest.FreeSpec
import metascala.Util
import java.lang.String
import scala.Predef.String

class ClassesTest extends FreeSpec {
  import Util._

  "classes" - {
    val tester = new VM()
    "customClass" in tester.test{
      val c1 = new Cow()
      val c2 = new Cow()
      c1.moo.length() + c2.moo.length()
    }
    "stringConcat" in tester.test{
      val x = new StringBuilder()
      x.append('a')
      x.toString()
    }
    "inheritance" in tester.test{
      val b = new Bull
      b.mooTwice
    }
    "constructor" in tester.test{
      val m = new Matrix(5, 7, -1, 3)
      m.determinant
    }
    "superConstructor" in tester.test{
      val m = new DoubleMatrix(2, 4, -8, 4)
      m.determinant
    }
    "override" in tester.test{
      val m = new DoubleDetMatrix(1, 2, 3, 4)
      m.determinant
    }
    "innerClass" in tester.test{
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
    "implement" in tester.run("implement", 10)
    "abstractClass" in tester.run("abstractClass")
    "shadowedInheritedGet" in tester.run("shadowedInheritedGet")
    "shadowedInheritedSet" in tester.run("shadowedInheritedSet")
    "superMethod" in tester.run("superMethod")
    "staticInheritance" in tester.run("staticInheritance")
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

