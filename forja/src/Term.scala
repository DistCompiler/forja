package forja

import scala.annotation.publicInBinary
import scala.compiletime.erasedValue

final class Term[T] @publicInBinary private[forja] (private[forja] val erasedMeta: Lang2.TermMeta[T]) extends AnyVal:
  extension [C](C: C)(using Lang2.IsCompanion[C, T])(using meta_ : Lang2.TermMeta[T])
    transparent inline def meta: meta_.type = meta_

    export meta.*

    transparent inline def M: Lang2.TermMeta.Appl[C, T] =
      inline erasedValue[meta_.Cases] match
        case _: EmptyTuple =>
          new Lang2.TermMeta.Appl0[C, T](meta_)
        case _: Tuple1[t] =>
          new Lang2.TermMeta.ApplN[C, T, t, t](meta_)
        case _ =>
          new Lang2.TermMeta.ApplN[C, T, meta_.Cases, meta_.Cases](meta_)
      end match
    end M
  end extension
end Term

object Term:
  inline def derived[T]: Term[T] =
    ${ MetaMacros.termDerivedImpl[T] }
  end derived
end Term
