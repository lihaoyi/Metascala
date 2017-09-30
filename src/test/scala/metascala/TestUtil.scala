package metascala

object TestUtil {


  def assertEquals(svmRes: Any, refRes: Any) = (svmRes, refRes)  match{
    case (a: Double, b: Double) if java.lang.Double.isNaN(a) && java.lang.Double.isNaN(b) =>
    case (a: Float, b: Float) if java.lang.Float.isNaN(a) && java.lang.Float.isNaN(b) =>
    case (a: Array[_], b: Array[_]) if a.length == b.length && a.sameElements(b) =>
    case (a: Array[AnyRef], b: Array[AnyRef]) if a.length == b.length && java.util.Arrays.deepEquals(a, b) =>
    case _ => assert(svmRes == refRes, s"svmRes: $svmRes, refRes: $refRes")
  }
  implicit class DoStuff(val vm: VM) {

    def test[T: VReader](thunk: => T) = {
      assertEquals(vm.exec(thunk), thunk)
    }
    def testSafe[T: VReader](thunk: => T) = {
      assertEquals(vm.execSafe(thunk), thunk)
    }

    def testFunc[A, B, R: VReader](t: (A, B) => R)(a: A, b: B) = {
      testFuncBase(t, Seq(a, b))
    }
    def testFunc[A, R: VReader](t: (A) => R)(a: A) = {
      testFuncBase(t, Seq(a))
    }
    def testFunc[R: VReader](t: () => R) = {
      testFuncBase(t, Nil)
    }

    def testFuncBase[T: VReader](t: Any, args: Seq[Any]): Unit = {
      val path = t.getClass.getName.replace('.', '/')
//      println(path)
//      println(getClass.getResourceAsStream(path + ".class"))
//      println("args " + args)


      val method = t.getClass
        .getMethods()
        .find(_.getName == "apply")
        .get

      val svmRes = vm.invokeSafe(path, "apply", Seq(t) ++ args.map(_.asInstanceOf[AnyRef]))

      val refRes = method.invoke(t, args.map(_.asInstanceOf[AnyRef]):_*)

      val inString = args.toString
//      println("svmRes " + svmRes)
//      println("refRes " + refRes)

      try assertEquals(svmRes, refRes)
      catch {case ex: utest.AssertionError =>
        println("Test failed for input")
        println(inString)

        throw ex
      }
    }
  }
  class ReflectiveRunner(className: String){
    def run(main: String, args: Any*) = {
      val method = java.lang.Class.forName(className)
        .getMethods()
        .find(_.getName == main)
        .get



      method.invoke(null, args.map(x => x.asInstanceOf[AnyRef]):_*)
    }
  }
  class Tester(className: String, memorySize: Int = 1 * 1024 * 1024){

    val svm = new VM(memorySize=memorySize)

    val ref = new ReflectiveRunner(className)
    def run[T: VReader](main: String, args: Any*) = {

      val refRes = ref.run(main, args:_*)
      val svmRes = svm.invokeSafe[T](className.replace('.', '/'), main, args)
      val inString = args.toString
//      println("svmRes " + svmRes)
//      println("refRes " + refRes)
//      println("args " + args)
      try assertEquals(svmRes, refRes)
      catch {case ex: utest.AssertionError =>
        println("Test failed for input")
        println(inString)

        throw ex
      }
    }
  }

}

