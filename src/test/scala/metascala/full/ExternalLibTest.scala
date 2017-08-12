package metascala.full

import metascala.Util._
import metascala.VM
import org.scalatest.FreeSpec
class ExternalLibTest extends FreeSpec {

  val tester = new VM()
  "fansi" in tester.test{
    (fansi.Color.Red("Hello") ++ fansi.Color.Red("World"))
      .overlay(fansi.Underlined.On, 3, 7)
      .render
  }
//  "fastparse" in tester.test{
//    import fastparse.all._
//    val p = "hello" ~ "world"
//    p.parse("hello world")
//  }

}

