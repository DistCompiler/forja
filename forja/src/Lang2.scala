package forja

import scala.quoted.Type
import scala.annotation.publicInBinary
import forja.util.TrivialMatch
import scala.compiletime.Erased

trait Lang2:

end Lang2

object Lang2:
  sealed trait IsNode[N] extends Erased:
    type L <: Lang2
    type Path <: Tuple
  end IsNode

  object IsNode:
    final class Aux[N, L0 <: Lang2, Path0 <: Tuple] extends IsNode[N]:
      type L = L0
      type Path = Path0
    end Aux

    transparent inline given instance: [N] => (inline ev: Term[N] | Sum[N]) => Aux[N, ?, ?] =
      ${ MetaMacros.isNodeImpl[N] }
    end instance
  end IsNode

  final class IsCompanion[C, T] extends Erased

  object IsCompanion:
    transparent inline given instance: [C, T] => IsCompanion[C, T] =
      ${ MetaMacros.isCompanionImpl[C, T] }
    end instance
  end IsCompanion

  sealed trait LaunderNode[L2 <: Lang2, N1]:
    type N2
  end LaunderNode

  object LaunderNode:
    final class Aux[L2 <: Lang2, N1, N20] extends LaunderNode[L2, N1]:
      type N2 = N20
    end Aux

    transparent inline given instance: [L2 <: Lang2, N1] => (isn: IsNode[N1]) => Aux[L2, N1, ?] =
      ${ MetaMacros.launderNodeImpl[isn.Path, L2, N1] }
    end instance
  end LaunderNode

  final class HasSameMeta[N1, N2] extends Erased

  object HasSameMeta:
    transparent inline given instance: [N1, N2] => (i1: IsNode[N1]) => (i2: IsNode[N2]) => (launder: LaunderNode[i2.L, N1]) => (inline ev: launder.N2 =:= N2) => HasSameMeta[N1, N2] =
      ${ MetaMacros.hasSameMetaImpl[N1, N2, i1.L, i2.L] }
    end instance
  end HasSameMeta

  sealed trait NodeMeta[T]:
    type Companion <: Singleton
    def Companion: Companion
    
    type Cases <: Tuple
  end NodeMeta

  trait TermMeta[T] extends NodeMeta[T]:
    type Labels <: Tuple

    @publicInBinary
    private[forja] def applyImpl(arg: Any): T

    @publicInBinary
    private[forja] def unapplyImpl(t: T): Any
  end TermMeta

  object TermMeta:
    sealed trait Appl[C, T] extends Any

    final class Appl0[C, T](private val meta: TermMeta[T]) extends AnyVal, Appl[C, T]:
      inline def apply(): T =
        meta.applyImpl(().asInstanceOf)
      end apply

      def unapply(t: T): true = true
    end Appl0

    final class ApplN[C, T, ApplyArg, UnapplyResult](private val meta: TermMeta[T]) extends AnyVal, Appl[C, T]:
      inline def apply(arg: ApplyArg): T =
        meta.applyImpl(arg)
      end apply

      def unapply(t: T): TrivialMatch[UnapplyResult] =
        TrivialMatch(meta.unapplyImpl(t).asInstanceOf[UnapplyResult])
      end unapply
    end ApplN
  end TermMeta

  trait SumMeta[T] extends NodeMeta[T]:
    extension (T: T)
      def ordinal: Int
    end extension

    @publicInBinary
    private[forja] def applyImpl(idx: Int, elem: ErasedNode): T
  end SumMeta

  object SumMeta:
    sealed trait Appl[C, T] extends Any

    final class ApplN[C, T, Cases <: Tuple](private val meta: SumMeta[T]) extends AnyVal, Appl[C, T]:
      inline def apply(arg: Tuple.Union[Cases]): T =
        ${ MetaMacros.sumApplyImpl[Cases, T]('{ meta }, '{ arg }) }
      end apply

      def unapply(t: T): Tuple.Union[Cases] =
        ???
      end unapply
    end ApplN
  end SumMeta

  trait Impl[L <: Lang2]:
    transparent inline given termMeta: [T] => (term: Term[T]) => (isn: IsNode[T]) => (inline ev: isn.L <:< L) => TermMeta[T] =
      ${ MetaMacros.termMetaImpl[L, T]('{ term }) }
    end termMeta

    transparent inline given sumMeta: [T] => (sum: Sum[T]) => (isn: IsNode[T]) => (inline ev: isn.L <:< L) => SumMeta[T] =
      ${ MetaMacros.sumMetaImpl[L, T]('{ sum }) }
    end sumMeta
  end Impl

  trait Ext:
    L: Lang2 =>

  end Ext

  private[forja] trait ErasedNode:

  end ErasedNode
end Lang2

