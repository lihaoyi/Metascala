package metascala.full



import metascala.Util._
import metascala.VM
import org.scalatest.FreeSpec

object ExternalLibTest{
  def func(f: Object) = f
}
class ExternalLibTest extends FreeSpec {

  val tester = new VM()
  "fansi" in tester.test{
    (fansi.Color.Red("Hello") ++ fansi.Color.Red("World"))
      .overlay(fansi.Underlined.On, 3, 7)
      .render
  }


  "fastparse-regression" in tester.test{
    // Minimized repro for a bug found in trying to get fastparse
    // to run. In the old code, this failed because we were only propagating
    // the return value of function calls to a single output register, rather
    // than propagating it to all relevant output registers
    val c = 1
    val o = ExternalLibTest
    val res =
      if (0 >= c) o
      else 1 match {case f: Int => ExternalLibTest.func(o)}

    res == null
  }


  "fastparse" in tester.test{
    import fastparse.all._
    val p = P( ("Hello" ~ " ".rep(1) ~ "world").! ~ "!".!.?)
    val res1 = p.parse("Helloworld!").asInstanceOf[Parsed.Failure]
    val res2 = p.parse("Hello    world").asInstanceOf[Parsed.Success[String]]
    val res3 = p.parse("Hello world!").asInstanceOf[Parsed.Success[String]]

    (res1.index, res2.value, res3.value).toString
  }

  "scalatags" in tester.test{
    import scalatags.Text.all._
    val fragment = html(
      head(
        script(src:="..."),
        script(
          "alert('Hello World')"
        )
      ),
      body(
        div(
          h1(id:="title", "This is a title"),
          p("This is a big paragraph of text")
        )
      )
    )

    fragment.render
  }

  "pprint" in tester.test{
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
}

