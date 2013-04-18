package metascala

import imm.Type.Prim.Info
import reflect.ClassTag

/**
 * This vrt contains virtual representations of the data within the java
 * virtual machine, as well as implicit conversions back and forth. Even for
 * data for which the wrappers are simple boxes (e.g. for primitive types) this
 * allows us to keep the "real" data types (which subclass Any) neatly
 * separated from the virtual data types (which subclass vrt.Val).
 *
 * This lets us use implicit conversions to neatly enforce that the proper
 * conversions are being done whenever data is transferred from the outside
 * world into the virtual world.
 */
package object vrt {
  import scala._
  lazy val unsafe = {
    val field = Class.forName("sun.misc.Unsafe").getDeclaredField("theUnsafe")
    field.setAccessible(true)
    field.get(null).asInstanceOf[sun.misc.Unsafe]
  }

}

