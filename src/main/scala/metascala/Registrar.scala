package metascala

class Registrar(f: Ref => Unit, val vm: VMInterface) extends Function1[Ref, Unit]{
  def apply(i: Ref) = f(i)
}
