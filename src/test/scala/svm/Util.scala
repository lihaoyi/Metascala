package svm

import java.io.{IOException, DataInputStream}
import java.util
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.exceptions.TestFailedException
import scala.util.Random


object Gen{
  val conforms = null
  def check[T1](test: T1 => Unit, n: Int)(implicit gen1: Int => T1) = {
    for (i <- 0 until n){
      test(gen1(i))
    }
  }
  def check[T1, T2](test: (T1, T2) => Unit, n: Int )(implicit gen1: Int => T1, gen2: Int => T2) = {
    for (i <- 0 until n){
      test(gen1(i), gen2(i))
    }
  }
  implicit val intAll = (i: Int) => Random.nextInt()

  implicit val longAll = (i: Int) =>Random.nextInt().toLong << 32 + Random.nextInt()

  def int(bits: Int) = (i: Int) => {
    println("Gen Int")
    println(bits)
    println(1 << bits)
    val res = Random.nextInt(1 << bits)
    println(res)
    res

  }
  def long(bits: Int) = (i: Int) => {
    if (bits >= 32){
      (Random.nextInt(1 << (bits - 32)).toLong << 32) | Random.nextInt(32)
    } else {
      Random.nextInt(1 << bits)
    }

  }
  implicit val GenFloat = (i: Int) => Random.nextFloat()
  implicit val GenDouble = (i: Int) => Random.nextDouble()
  implicit val GenBoolean = (i: Int) => Random.nextBoolean()

}

object Util{
  def loadClass(name: String) = {
    val slashName = s"/${name.replace(".", "/")}.class"
    println("loading class " + slashName)
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
      java.lang.Class.forName(className)
               .getMethods()
               .find(_.getName == main)
               .get
               .invoke(null, args.map(x => x.asInstanceOf[AnyRef]):_*)
    }
  }
  class Tester(className: String){
    val svm = new SingleClassVM(className, Util.loadClass)
    val ref = new ReflectiveRunner(className)
    def run(main: String, args: Any*) = {
      val svmRes = svm.run(main, args:_*)
      val refRes = ref.run(main, args:_*)
      val inString = args.toString
      try{
        svmRes should be === refRes
      }catch {case ex: TestFailedException =>
        println("Test failed for input")
        println(inString)
        println(svmRes)
        println(refRes)
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



