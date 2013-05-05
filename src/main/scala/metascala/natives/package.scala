package metascala

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
  def value[T](p: Prim[T])(nPop: Int, x: => T)(vt: rt.Thread): Unit = {
    for(i <- 0 until nPop) ???
    ///p.write(x, vt.push)
  }
  def noOp(nPop: Int) = (vt: rt.Thread) => for(i <- 0 until nPop) ???

  /**
   * Implements a nice DSL to build the list of trapped method calls
   */
  implicit class pimpedRoute(val m: Seq[(String, Any)]) extends AnyVal{
    def toRoute(parts: List[String] = Nil): Seq[rt.Method.Native] = {
      m.flatMap{ case (k, v) =>
        v match{
          case thing: Seq[(String, Any)] =>
            thing.toRoute(k :: parts).map {
              case rt.Method.Native(clsName, sig, func) => rt.Method.Native(if (clsName == "") k else k + "/" + clsName, sig, func)
            }

          case func: (`rt`.Thread => Unit) =>
            val (name, descString) = k.splitAt(k.indexOf('('))
            val p = parts.reverse

            val fullDescString =
              descString
                .replaceAllLiterally("L//", s"L${p(0)}/${p(1)}/")
                .replaceAllLiterally("L/", s"L${p(0)}/")

            val desc = imm.Desc.read(fullDescString)
            Vector(rt.Method.Native("", imm.Sig(name, desc), func))
        }
      }
    }
  }
  implicit class pimpedMap(val s: String) extends AnyVal{
    def /(a: (String, Any)*) = s -> a
    def x(a: rt.Thread => Unit) = s -> a
  }
}
