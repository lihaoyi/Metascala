package metascala.full

import metascala.{BufferLog, Util}
import org.scalatest.FreeSpec
import org.scalatest._

object ScalaLib{
  def hello = "hello"
  def lists(n: Int) = {
    0 to n
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
    lazy val fs: Stream[BigInt] =
      0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)

    fs.view.takeWhile(_.toString.length < n).size
  }
}
class ScalaLib extends FreeSpec with Util{
  val buffer = new BufferLog(4000)
  val tester = new Tester("metascala.full.ScalaLib")
  "hello world" in tester.run("hello")
  "lists" in tester.run("lists", 5)
  "palindrome" in {
    tester.run("palindrome", 100, 130)
  }
  "bigFibonacci" in {
    tester.run("bigFibonacci", 100)
  }
}
