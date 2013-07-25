package metascala.full

import metascala.{BufferLog, Util}
import org.scalatest.FreeSpec
import org.scalatest._
import scala.collection.immutable.Range.Inclusive
import scala.collection.immutable.Range
import scala.runtime.RichInt
import scala.concurrent.{Await, Promise}


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
    lazy val fs: Stream[BigInt] =
      0 #:: 1 #:: fs.zip(fs.tail).map(p => p._1 + p._2)

    fs.view.takeWhile(_.toString.length < n).size

  }

  def futures = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val a = Promise[Promise[Int]]()
    val b = Promise[List[String]]()
    val c = for{
      ar <- a.future
      br <- b.future
      i <- ar.future
    } yield i.toString :: br
    a.success(Promise.successful(1))
    b.success(List("2", "3", "4"))
    import scala.concurrent.duration._
    Await.result(c, 10 seconds)
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
    tester.run("bigFibonacci", 100)
  }
//  "futures" in {
//    tester.run("futures", 100)
//  }
  "lol" in{
    tester.run("lol")
  }
}
