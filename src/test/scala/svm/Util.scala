package svm

import java.io.{IOException, DataInputStream}
import java.util
import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.exceptions.TestFailedException
import scala.util.Random


object Gen{

  def check[T1: Function0](test: T1 => Unit) = {
    for (i <- 0 until 10){
      test(implicitly[Function0[T1]].apply())
    }
  }
  def check[T1: Function0, T2: Function0](test: (T1, T2) => Unit) = {
    for (i <- 0 until 10){
      val a = implicitly[Function0[T1]].apply()
      val b = implicitly[Function0[T2]].apply()
      println(a + " " + b)
      test(a, b)
    }
  }
  implicit val intAll = () => Random.nextInt()

  implicit val longAll = () =>Random.nextInt().toLong << 32 + Random.nextInt()

  def int(bits: Int) = () => {
    println("Gen Int")
    println(bits)
    println(1 << bits)
    val res = Random.nextInt(1 << bits)
    println(res)
    res

  }
  def long(bits: Int) = () => {
    if (bits >= 32){
      (Random.nextInt(1 << (bits - 32)).toLong << 32) | Random.nextInt(32)
    } else {
      Random.nextInt(1 << bits)
    }

  }
  implicit val GenFloat = () => Random.nextFloat()
  implicit val GenDouble = () => Random.nextDouble()
  implicit val GenBoolean = () => Random.nextBoolean()

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



