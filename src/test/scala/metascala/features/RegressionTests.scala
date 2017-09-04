package metascala.features

import java.io.{ByteArrayOutputStream, PrintWriter}
import java.security.AccessController
import java.util
import java.util.PropertyPermission

import metascala.VM
import metascala.full.ExternalLibTest
import org.scalatest.FreeSpec
import metascala.TestUtil._
import sun.security.action.GetPropertyAction

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
  "printwriter" in tester.test{
    val baos = new ByteArrayOutputStream()
    val pw = new PrintWriter(baos)
    pw.print("Hello")
    pw.print("World")
    new String(baos.toByteArray)

  }
  "invokeDynamic" in {
    // Exposed a problem where we were unnecessarily de-virtualizing the
    // result of methods invoked from native methods, which causes problems
    // when they return things like java.lang.Class instances which cannot
    // be de-virtualized
    val tester = new VM()
    tester.test {
      val key = "jdk.internal.lambda.dumpProxyClasses"
      val action = new GetPropertyAction(key)
      val permission = new PropertyPermission(key, "read")
      AccessController.doPrivileged(action, null, permission)
    }
  }
  "ConcurrentHashMapWithLoadFactor" in tester.test{
    // Exposed a bug where the d2l bytecode instruction had the wrong
    // return-type, causing the resulting value to be accidentally coerced
    // between Long -> Float -> Long, resulting in an invalid outcome.
    val x = new java.util.concurrent.ConcurrentHashMap[Int, Int](16, 0.75f)
    x.put(1, 1)
  }
  "EnumMapPutIfAbsent" in tester.test{
    // Didn't work if EnumMap#getKeyUniverse was broken, or we weren't properly
    // looking up interface default methods inherited through superclasses
    val x = new util.EnumMap[java.awt.Desktop.Action, Int](classOf[java.awt.Desktop.Action])
    x.putIfAbsent(java.awt.Desktop.Action.BROWSE, 1)
  }
  "tryCatchRanges" in {
    // This used to fail because we were not properly computing and handling
    // try-catch ranges, resulting in the exception being thrown in this
    // snippet propagating uncaught out of the VM.
    val tester = new VM()
    tester.test{
      val script = 1
      val obj =
        try throw ThrownEx
        catch{case e: Throwable => e.getMessage}
    }
  }
  "clone" in {
    // Make sure .clone() works properly
    val tester = new VM()
    tester.test{
      val x = Array(true)
      val y = x.clone
      y(0) = false
      x(0)
    }
  }
}

object ThrownEx extends Exception()
