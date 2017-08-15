package metascala.features

import java.io.{ByteArrayOutputStream, PrintWriter}
import java.security.AccessController
import java.util.PropertyPermission
import java.util.concurrent.ConcurrentHashMap

import metascala.VM
import metascala.full.ExternalLibTest
import org.scalatest.FreeSpec
import metascala.TestUtil._
import sun.security.action.GetPropertyAction
import sun.util.locale.BaseLocale

import scala.sys.BooleanProp

/**
  * Created by lihaoyi on 12/8/17.
  */
class RegressionTests extends FreeSpec{
  val tester = new VM(memorySize = 15 * 1014 * 1024)

//  "doubleArrayOps" in tester.test{
//    // Minimized repro for a bug caused by advancePc() and gathering of
//    // InvokeVirtual arguments happening in the wrong order resulting in
//    // NullPointerExceptions
//    if ("A" eq "B") ()
//    else BooleanProp.toString != null
//  }
//
//  "fastparse" in tester.test{
//    // Minimized repro for a bug found in trying to get fastparse
//    // to run. In the old code, this failed because we were only propagating
//    // the return value of function calls to a single output register, rather
//    // than propagating it to all relevant output registers
//    val c = 1
//    val o = ExternalLibTest
//    val res =
//      if (0 >= c) o
//      else 1 match {case f: Int => ExternalLibTest.func(o)}
//
//    res == null
//  }
//  "printwriter" in tester.test{
//    val baos = new ByteArrayOutputStream()
//    val pw = new PrintWriter(baos)
//    pw.print("Hello")
//    pw.print("World")
//    new String(baos.toByteArray)
//
//  }
//  "invokeDynamic" in {
//    // Exposed a problem where we were unnecessarily de-virtualizing the
//    // result of methods invoked from native methods, which causes problems
//    // when they return things like java.lang.Class instances which cannot
//    // be de-virtualized
//    val tester = new VM()
//    tester.test {
//      val key = "jdk.internal.lambda.dumpProxyClasses"
//      val action = new GetPropertyAction(key)
//      val permission = new PropertyPermission(key, "read")
//      AccessController.doPrivileged(action, null, permission)
//    }
//  }
  "locale" in tester.test{
    BaseLocale.createInstance("en", "")
  }
}

