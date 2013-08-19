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
import java.util
import scala.collection.JavaConversions._
import org.mozilla.javascript.Context

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
    val stream = new DataInputStream(loaded)
    val bytes = new Array[Byte](stream.available())
    stream.readFully(bytes)
    val cr = new ClassReader(bytes)
    val classNode = new ClassNode()

    cr.accept(classNode, ClassReader.EXPAND_FRAMES)
    val s = classNode.name;
    val methods = for(m <- classNode.methods) yield {
      m.name + m.desc
    }
    s + "\n" + methods.mkString("\n")
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
    println("AAA")
    val ctx = Context.enter()
    println("BBB")
    val scope = ctx.initStandardObjects()
    println("CCC")
    println(ctx.evaluateString(scope, "1 + 1", "<cmd>", 0, null))
    println("DDD")
    0
  }
}
class ScalaLib extends FreeSpec {
  import Util._
  val buffer = new BufferLog(4000)

  val tester = new Tester("metascala.full.ScalaLib", memorySize = 128 * 1024 * 1024, log=buffer)
  "hello world" in tester.run("hello")
  "predef" in tester.run("predef", 5)
  "palindrome" in tester.run("palindrome", 100, 130)
  "bigFibonacci" in tester.run("bigFibonacci", 30)
  "parseClass" in tester.run("parseClass")
//  "lol" in tester.run("lol")

}
