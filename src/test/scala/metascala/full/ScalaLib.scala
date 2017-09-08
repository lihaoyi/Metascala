package metascala
package full

import utest._
import scala.concurrent.{Await, Promise}
import java.io.DataInputStream
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode
import scala.collection.JavaConversions._
import org.mozilla.javascript.Context
import TestUtil._
object ScalaLib extends utest.TestSuite {

  def parseClass() = {

    val slashName = "/java/lang/Object.class"

    val loaded = getClass.getResourceAsStream(slashName)
    val stream = new DataInputStream(loaded)
    val bytes = new Array[Byte](stream.available())
    stream.readFully(bytes)
    val cr = new ClassReader(bytes)
    val classNode = new ClassNode()

    cr.accept(classNode, ClassReader.EXPAND_FRAMES)
    val s = classNode.name;
    val methods = for (m <- classNode.methods) yield {
      m.name + m.desc
    }
    s + "\n" + methods.mkString("\n")
  }

  def tests = this {
    val buffer = new BufferLog(4000)

    val tester = new VM(memorySize = 64 * 1024)

    "predef" - {
      val x = 5
      tester.test {
        Predef
        0
      }
    }
    "palindrome" - {
      val min = 100
      val max = 130
      tester.test {
        def isPalindrome(s: String): Boolean = s.reverse.mkString == s

        val palindromes = for {
          a <- min until max
          b <- a until max
          p = a * b
          if isPalindrome(p.toString)
        } yield p
        palindromes

      }
    }
    "bigFibonacci" - {
      val n = 10
      tester.test {
        lazy val fs: Stream[BigInt] =
          0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)

        fs.view.takeWhile(_.toString.length < n).size
      }
    }
    "Euler1" - {
      tester.test {
        val r = (1 until 1000).view.filter(n => n % 3 == 0 || n % 5 == 0).sum
        r
      }

    }
    "Euler2" - {
      tester.test {
        lazy val fs: Stream[Int] = 0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)

        val r = fs.view.takeWhile(_ <= 4000000).filter(_ % 2 == 0).sum
        r
      }
    }
    "Euler4" - {
      tester.test {
        (10 to 99).view
          .flatMap(i => (i to 99).map(i *))
          .filter(n => n.toString == n.toString.reverse)
          .max
      }
    }
    "Euler10" - {
      tester.test {
        lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
          ps.takeWhile(j => j * j <= i).forall(i % _ > 0))
        ps.view.takeWhile(_ < 2000).foldLeft(0L)(_ + _)
      }
    }

    "Euler16" - {
      tester.test {
        BigInt(2).pow(1000).toString.view.map(_.asDigit).sum
      }
    }
    "Euler29" - {
      tester.test {
        (2 to 10).flatMap(a => (2 to 10)
          .map(b => BigInt(a).pow(b)))
          .distinct
          .size

      }
    }
  }
}