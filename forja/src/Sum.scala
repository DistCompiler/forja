package forja

final class Sum[T](private[forja] val erasedMeta: Lang2.SumMeta[T]) extends AnyVal:
  extension [C](C: C)(using Lang2.IsCompanion[C, T])(using meta_ : Lang2.SumMeta[T])
    transparent inline def meta: meta_.type = meta_

    export meta.*

    transparent inline def M: Lang2.SumMeta.ApplN[C, T, meta_.Cases] =
      new Lang2.SumMeta.ApplN(meta_)
    end M
  end extension
end Sum

object Sum:
  inline def derived[T]: Sum[T] =
    ${ MetaMacros.sumDerivedImpl[T] }
  end derived
end Sum
