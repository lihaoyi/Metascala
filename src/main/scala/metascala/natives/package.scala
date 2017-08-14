package metascala
import imm.Type.Prim
import collection.mutable
/**
 * `natives` contains the bindings between method calls internal to the Metascala
 * VM and external functionality of the host JVM. It defines bindings in a
 * hierarchical fashion, as well as a default set of bindings in Default.Scala.
 *
 * A Metascala VM takes a set of Bindings on instantiation, which can be customized
 * e.g. to trapped the calls to native methods and redirect them to wherever you want.
 *
 */
package object natives {

}
