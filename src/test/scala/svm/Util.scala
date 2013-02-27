package svm

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


  def int(bits: Int) = {
    println("Gen Int")
    println(bits)
    println(1 << bits)
    val res = Random.nextInt(1 << bits)
    println(res)
    res

  }
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
    if (loaded == null) throw new IOException("Can't find file " + slashName)
    val stream = new DataInputStream(loaded)
    val bytes = new Array[Byte](stream.available())
    stream.readFully(bytes)
    //model.Util.printClass(bytes)
    bytes
  }
}
trait Util extends ShouldMatchers { this: FreeSpec  =>

  class SingleClassVM(className: String, val classLoader: String => Array[Byte])
  extends VirtualMachine(classLoader){
    def run(main: String, args: Any*): Any = invoke(className.replace('.', '/'), main, args)
  }
  class ReflectiveRunner(className: String){
    def run(main: String, args: Any*) = {
      val method = java.lang.Class.forName(className)
        .getMethods()
        .find(_.getName == main)
        .get

      println("Invoking Method " + method.getName + " with " + method.getParameterTypes.length + " " + args.length)

      method.invoke(null, args.map(x => x.asInstanceOf[AnyRef]):_*)
    }
  }
  class Tester(className: String){
    val svm = new SingleClassVM(className, Util.loadClass)
    val ref = new ReflectiveRunner(className)
    def run(main: String, args: Any*) = {
      val svmRes = svm.run(main, args:_*)
      val refRes = ref.run(main, args:_*)
      val inString = args.toString
      println(svmRes)
      println(refRes)
      try{
        svmRes should be === refRes
      }catch {case ex: TestFailedException =>
        println("Test failed for input")
        println(inString)

        throw ex
      }
    }
    def runC(main: String, args: => Seq[Any] = Nil) = {
      val svmRes = svm.run(main, args:_*)
      val refRes = ref.run(main, args:_*)
      println(svmRes)
      println(refRes)
      svmRes should be === refRes
    }
  }


}



