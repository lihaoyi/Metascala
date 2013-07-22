package metascala.full

import metascala.{BufferLog, Util}
import org.scalatest.FreeSpec
import org.scalatest._
import scala.collection.immutable.Range.Inclusive
import scala.collection.immutable.Range
import scala.runtime.RichInt


object ScalaLib{
  def hello = "hello"
  def predef(n: Int) = {
    Predef
    0
  }
  def palindrome(min: Int, max: Int) = {
    def isPalindrome(s: String): Boolean = s.reverse.mkString == s
    val palindromes = for {
      a <- (min until max)
      b <- (a until max)
      p = a*b
      if isPalindrome(p.toString)
    } yield p
    palindromes
  }

  def bigFibonacci(n: Int) = {
//    lazy val fs: Stream[BigInt] =
//      0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)
//
//    fs.view.takeWhile(_.toString.length < n).size
    java.lang.Long.toString(1)
  }
  def lol = {
    val args2: String = java.security.AccessController.doPrivileged(new sun.security.action.GetPropertyAction("java.security.auth.debug"))
    args2
  }
}
class ScalaLib extends FreeSpec with Util{
  val buffer = new BufferLog(4000)
  val tester = new Tester("metascala.full.ScalaLib", buffer)
  "hello world" in tester.run("hello")
  "predef" in tester.run("predef", 5)
  "palindrome" in {
    tester.run("palindrome", 100, 130)
  }
  "bigFibonacci" in {
    tester.run("bigFibonacci", 3)
  }
  "lol" in{
    try tester.run("lol")
    catch{case e =>
      buffer.lines.foreach(println)
      throw e
    }
  }
}
