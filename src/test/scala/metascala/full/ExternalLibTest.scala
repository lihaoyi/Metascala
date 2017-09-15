package metascala.full




import metascala.TestUtil._
import metascala.VM
import utest._

object ExternalLibTest extends utest.TestSuite {
  def func(f: Object) = f
  def tests = Tests {
    val tester = new VM(memorySize = 256 * 1024)
    "fansi" - tester.test {
      (fansi.Color.Red("Hello") ++ fansi.Color.Red("World"))
        .overlay(fansi.Underlined.On, 3, 7)
        .render
    }

    "fastparse" - tester.test {
      import fastparse.all._
      val p = P(("Hello" ~ " ".rep(1) ~ "world").! ~ "!".!.?)
      val res1 = p.parse("Helloworld!").asInstanceOf[Parsed.Failure]
      val res2 = p.parse("Hello    world").asInstanceOf[Parsed.Success[String]]
      val res3 = p.parse("Hello world!").asInstanceOf[Parsed.Success[String]]

      (res1.index, res2.value, res3.value).toString
    }

    "scalatags" - tester.test {
      import scalatags.Text.all._
      val fragment = html(
        head(
          script(src := "..."),
          script(
            "alert('Hello World')"
          )
        ),
        body(
          div(
            h1(id := "title", "This is a title"),
            p("This is a big paragraph of text")
          )
        )
      )

      fragment.render
    }

    "pprint" - tester.test {
      pprint.apply(
        List(
          List(1, 2, 3),
          Some(
            Array(2)
          ),
          Tuple2(
            Seq(1, 2, 3, 4),
            Map(
              1 -> 2,
              3 -> Array(4)
            )
          )
        )
      )
    }

    //  "javac" - {
    //    val tester = new VM(memorySize = 15 * 1014 * 1024, initializeStdout = true)
    //    tester.test{
    //      println(metascala.DummyCharset.getValue)
    ////      val pw = new PrintWriter(new ByteArrayOutputStream())
    //      val compiler = new MemoryJavaCompiler()
    ////      val source =
    ////        "public final class Solution {\n" +
    ////          "public static String greeting(String name) {\n" +
    ////          "\treturn \"Hello \" + name;\n" +
    ////          "}\n}\n"
    ////      val greeting = compiler.compileStaticMethod("greeting", "Solution", source)
    ////      val result = greeting.invoke(null, "soulmachine")
    ////      println(result.toString)
    //    }
    //  }
    "rhino" - {
      val tester = new VM(memorySize = 1 * 1024 * 1024, initializeStdout = true)
      tester.test {

        val cx = org.mozilla.javascript.Context.enter()
        val scope = cx.initStandardObjects()
        val script = "(function(suffix){ return 'hello ' + suffix})('world')"
        val obj = cx.evaluateString(scope, script, "Test Script", 1, null)
        obj
      }
    }
    //  "rhinoFail" - {
    //    val tester = new VM(memorySize = 1 * 1024 * 1024, initializeStdout = true)
    //    tester.test{
    //      val cx = org.mozilla.javascript.Context.enter()
    //      val scope = cx.initStandardObjects()
    //      val script = "doesntExist"
    //      try cx.evaluateString(scope, script, "Test Script", 1, null).toString
    //      catch{case e: Throwable => e.getMessage}
    //    }
    //  }
  }
}
