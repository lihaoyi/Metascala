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

    def test[T](thunk: => T) = {
      assertEquals(vm.exec(thunk), thunk)
    }
    def testFunc[A, B, C, R](t: (A, B, C) => R)(a: A, b: B, c: C) = {
      func(t, Seq(a, b, c))
    }

    def testFunc[A, B, R](t: (A, B) => R)(a: A, b: B) = {
      func(t, Seq(a, b))
    }
    def testFunc[A, R](t: (A) => R)(a: A) = {
      func(t, Seq(a))
    }
    def testFunc[R](t: () => R) = {
      func(t, Nil)
    }
    def func(t: Any, args: Seq[Any]) = {
      val path = t.getClass.getName.replace('.', '/')
//      println(path)
//      println(getClass.getResourceAsStream(path + ".class"))
//      println("args " + args)


      val method = t.getClass
        .getMethods()
        .find(_.getName == "apply")
        .get

      val svmRes = vm.invoke(path, "apply", Seq(t) ++ args.map(_.asInstanceOf[AnyRef]))

      val refRes = method.invoke(t, args.map(_.asInstanceOf[AnyRef]):_*)

      val inString = args.toString
//      println("svmRes " + svmRes)
//      println("refRes " + refRes)

      try{
        (svmRes, refRes) match{
          case (x: Array[_], y: Array[_]) => assert(x.toSeq == y.toSeq)
          case _ => assert(svmRes == refRes)
        }

      }catch {case ex: utest.AssertionError =>
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

    val svm = new VM(memorySize=memorySize){
      def run(main: String, args: Any*): Any ={
        val res = invoke(className.replace('.', '/'), main, args)
        res
      }
    }

    val ref = new ReflectiveRunner(className)
    def run(main: String, args: Any*) = {

      val refRes = ref.run(main, args:_*)
      val svmRes = svm.run(main, args:_*)
      val inString = args.toString
//      println("svmRes " + svmRes)
//      println("refRes " + refRes)
//      println("args " + args)
      try{
        (svmRes, refRes) match{
          case (x: Array[_], y: Array[_]) => assert(x.toSeq == y.toSeq)
          case _ => assert(svmRes == refRes)
        }
      }catch {case ex: utest.AssertionError =>
        println("Test failed for input")
        println(inString)

        throw ex
      }
    }
  }

}

