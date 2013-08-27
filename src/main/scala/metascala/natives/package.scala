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


  /**
   * Implements a nice DSL to build the list of trapped method calls
   */
  implicit class pimpedRoute(val m: Seq[(String, Any)]) extends AnyVal{
    def toRoute(parts: List[String] = Nil): Seq[rt.Method.Native] = {
      m.flatMap{ case (k, v) =>
        v match{
          case thing: Seq[(String, Any)] =>
            thing.toRoute(k :: parts).map { case rt.Method.Native(clsName, sig, func) =>
              rt.Method.Native(if (clsName == "") k else k + "/" + clsName, sig, func)
            }
          case func: ((`rt`.Thread, () => Val, Int => Unit) => Unit) =>
            val (name, descString) = k.splitAt(k.indexOf('('))
            val desc = imm.Desc.read(descString)
            Vector(rt.Method.Native("", imm.Sig(name, desc), func))
        }
      }
    }
  }
  implicit class pimpedMap(val s: String) extends AnyVal{
    def /(a: (String, Any)*) = s -> a
    def func[T](out: Prim[T])(f: rt.Thread => T) = s -> {
      (t: rt.Thread, args: () => Val, ret: Int => Unit) =>
        out.write(f(t), ret)
    }
    def func[A, T](a: Prim[A], out: Prim[T])(f: (rt.Thread, A) => T) = s -> {
      (t: rt.Thread, args: () => Val, ret: Int => Unit) =>
        out.write(f(t, a.read(args)), ret)
    }
    def func[A, B, T](a: Prim[A], b: Prim[B], out: Prim[T])(f: (rt.Thread, A, B) => T) = s -> {
      (t: rt.Thread, args: () => Val, ret: Int => Unit) =>
        out.write(f(t, a.read(args), b.read(args)), ret)
    }
    def func[A, B, C, T](a: Prim[A], b: Prim[B], c: Prim[C], out: Prim[T])(f: (rt.Thread, A, B, C) => T) = s -> {
      (t: rt.Thread, args: () => Val, ret: Int => Unit) =>
        out.write(f(t, a.read(args), b.read(args), c.read(args)), ret)
    }
    def func[A, B, C, D, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], out: Prim[T])(f: (rt.Thread, A, B, C, D) => T) = s -> {
      (t: rt.Thread, args: () => Val, ret: Int => Unit) =>
        out.write(f(t, a.read(args), b.read(args), c.read(args), d.read(args)), ret)
    }
    def func[A, B, C, D, E, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], e: Prim[E], out: Prim[T])(f: (rt.Thread, A, B, C, D, E) => T) = s -> {
      (t: rt.Thread, args: () => Val, ret: Int => Unit) =>
        out.write(f(t, a.read(args), b.read(args), c.read(args), d.read(args), e.read(args)), ret)
    }
    def func[A, B, C, D, E, F, T](a: Prim[A], b: Prim[B], c: Prim[C], d: Prim[D], e: Prim[E], f: Prim[F], out: Prim[T])(func: (rt.Thread, A, B, C, D, E, F) => T) = s -> {
      (t: rt.Thread, args: () => Val, ret: Int => Unit) =>
        out.write(func(t, a.read(args), b.read(args), c.read(args), d.read(args), e.read(args), f.read(args)), ret)
    }
    def value[T](out: Prim[T])(x: => T) = func(out)(t => x)
  }
}
