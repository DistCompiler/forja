package forja

object TypeAdjust:
  sealed trait Effective[L <: Lang2, N] extends scala.compiletime.Erased:
    type Effective
  end Effective

  object Effective:
    final class Aux[L <: Lang2, N, Effective0] extends Effective[L, N]:
      type Effective = Effective0
    end Aux

    inline given ident: [L <: Lang2, N] => Ident[N] => Aux[L, N, N] = new Aux

    inline given node: [L <: Lang2, N] => (launder: Lang2.LaunderNode[L, N]) => Aux[L, N, launder.N2] =
      new Aux
    end node
  end Effective

  sealed trait Erased[N] extends scala.compiletime.Erased:
    type Erased
  end Erased

  object Erased:
    final class Aux[N, Erased0] extends Erased[N]:
      type Erased = Erased0
    end Aux

    inline given ident: [N] => Ident[N] => Aux[N, N] = new Aux

    inline given node: [N] => (inline ev: Term[N] | Sum[N]) => Aux[N, Lang2.ErasedNode] = new Aux
  end Erased

  final class Ident[N] extends scala.compiletime.Erased

  object Ident:
    inline given Ident[Boolean] = new Ident
    inline given Ident[Byte] = new Ident
    inline given Ident[Char] = new Ident
    inline given Ident[Short] = new Ident
    inline given Ident[Int] = new Ident
    inline given Ident[Long] = new Ident
    inline given Ident[Float] = new Ident
    inline given Ident[Double] = new Ident

    inline given Ident[String] = new Ident
    inline given Ident[Unit] = new Ident
  end Ident
end TypeAdjust
