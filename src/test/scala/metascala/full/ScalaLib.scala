package metascala.full

import metascala.{BufferLog, Util}
import org.scalatest.FreeSpec
import org.scalatest._

object ScalaLib{
  def hello = "hello"
  def lists(n: Int) = {
    0 to n
  }
}
class ScalaLib extends FreeSpec with Util{
  val buffer = new BufferLog(1000)
  val tester = new Tester("metascala.full.ScalaLib", buffer)
  "hello world" in tester.run("hello")
  "lists" in {
    try tester.run("lists") catch {case e =>
      buffer.lines.foreach(println)
      throw e
    }
  }
}
