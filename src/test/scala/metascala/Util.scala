package metascala

import java.io.{IOException, DataInputStream}
import java.util
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.exceptions.TestFailedException
import scala.util.Random


object Gen{
  val conforms = null

  def chk[T1](test: T1 => Unit)(implicit gen1: Iterable[T1]) = {
    gen1.foreach(test)

  }
  def chk[T1, T2](test: (T1, T2) => Unit)(implicit gen1: Iterable[T1], gen2: Iterable[T2]) = {
    for ((i, j) <- gen1 zip gen2){
      test(i, j)
    }
  }
  def chk[T1, T2, T3](test: (T1, T2, T3) => Unit)(implicit gen1: Iterable[T1], gen2: Iterable[T2], gen3: Iterable[T3]) = {
    for (((i, j), k) <- gen1 zip gen2 zip gen3){
      test(i, j, k)
    }
  }

  implicit class multiplyGen(i: Int){
    def **[A](a: => A) = Iterable.fill(i)(a)
  }
  def intAll = Random.nextInt()
  def floatAll: Float = {
    val x = java.lang.Float.intBitsToFloat(intAll)
    if (java.lang.Float.isNaN(x)) floatAll
    else x
  }
  def longAll = Random.nextLong()
  def doubleAll: Double = {
    val x = java.lang.Double.longBitsToDouble(longAll)
    if (java.lang.Double.isNaN(x)) doubleAll
    else x
  }


  def int(bits: Int) = Random.nextInt(1 << bits)

  def long(bits: Int) =  {
    if (bits >= 32){
      (Random.nextInt(1 << (bits - 32)).toLong << 32) | Random.nextInt(32)
    } else {
      Random.nextInt(1 << bits).toLong
    }

  }
  implicit val GenFloat = (i: Int) => Random.nextFloat()
  implicit val GenDouble = (i: Int) => Random.nextDouble()
  implicit val GenBoolean = (i: Int) => Random.nextBoolean()

}

object Util{
  def loadClass(name: String) = {
    val slashName = s"/${name.replace(".", "/")}.class"

    val loaded = getClass.getResourceAsStream(slashName)
    if (loaded == null) None
    else {
      val stream = new DataInputStream(loaded)
      val bytes = new Array[Byte](stream.available())
      stream.readFully(bytes)
      //imm.Util.printClass(bytes)
      Some(bytes)
    }
  }

  class SingleClassVM(className: String, log: (=>String) => Unit) extends VM(log = log){
    def run(main: String, args: Any*): Any ={
      val res = invoke(className.replace('.', '/'), main, args)
      res
    }
  }
}
trait Util extends ShouldMatchers { this: FreeSpec  =>


  class ReflectiveRunner(className: String){
    def run(main: String, args: Any*) = {
      val method = java.lang.Class.forName(className)
        .getMethods()
        .find(_.getName == main)
        .get



      method.invoke(null, args.map(x => x.asInstanceOf[AnyRef]):_*)
    }
  }
  class Tester(className: String, log: (=>String) => Unit = x => ()){

    implicit val svm = new Util.SingleClassVM(className, log)
    val ref = new ReflectiveRunner(className)
    def run(main: String, args: Any*) = {

      val svmRes = svm.run(main, args:_*)
      val refRes = ref.run(main, args:_*)
      val inString = args.toString
      println("svmRes " + svmRes)
      println("refRes " + refRes)
      println("args " + args)
      try{
        svmRes should be === refRes
      }catch {case ex: TestFailedException =>
        println("Test failed for input")
        println(inString)

        throw ex
      }
    }
  }


}
class BufferLog(size: Int, delay: Long  = 0) extends ((=> String) => Unit){
  val buffer = new Array[String](size)
  var index = 0
  var count = 0l
  def apply(s: =>String) = {

    count += 1
    if (count > delay){
      buffer(index) = s
      index = (index + 1) % size
    }
  }
  def lines = buffer.drop(index) ++ buffer.take(index)
}




