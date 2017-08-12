package metascala.features

import metascala.VM
import metascala.full.ExternalLibTest
import org.scalatest.FreeSpec
import metascala.Util._
import scala.sys.BooleanProp

/**
  * Created by lihaoyi on 12/8/17.
  */
class RegressionTests extends FreeSpec{
  val tester = new VM(memorySize = 15 * 1014 * 1024)

  "doubleArrayOps" in tester.test{
    // Minimized repro for a bug caused by advancePc() and gathering of
    // InvokeVirtual arguments happening in the wrong order resulting in
    // NullPointerExceptions
    if ("A" eq "B") ()
    else BooleanProp.toString != null
  }

  "fastparse" in tester.test{
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

}
