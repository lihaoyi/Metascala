package metascala.full

import metascala.{imm, BufferLog, Util}
import org.scalatest.FreeSpec
import org.scalatest._
import scala.collection.immutable.Range.Inclusive
import scala.collection.immutable.Range
import scala.runtime.RichInt
import scala.concurrent.{Await, Promise}
import java.io.DataInputStream
import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.ClassNode


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
  def main(args: Array[String]){
    println(parseClass())
  }
  def parseClass() = {

    val slashName = "/java/lang/Object.class"

    val loaded = getClass.getResourceAsStream(slashName)
    println("loaded " + loaded)

    val stream = new DataInputStream(loaded)
    println("A")
    val bytes = new Array[Byte](stream.available())
    println("B")
    stream.readFully(bytes)
    println("C")
    val cr = new ClassReader(bytes)
    val classNode = new ClassNode()

    cr.accept(classNode, ClassReader.EXPAND_FRAMES)
    println("D")
    classNode.name


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
  val tester = new Tester("metascala.full.ScalaLib", memorySize = 128 * 1024 * 1024)
  "hello world" in tester.run("hello")
  "predef" in tester.run("predef", 5)
  "palindrome" in tester.run("palindrome", 100, 130)
  "bigFibonacci" in tester.run("bigFibonacci", 100)
  "parseClass" in tester.run("parseClass")
  "lol" in tester.run("lol")

}
