package forja.util

import scala.compiletime.Erased

abstract class InlineConversion[-T, +U] extends Conversion[T, U], Erased:
  inline def apply(t: T): U
end InlineConversion

object InlineConversion:
  final class ByCast[-T, +U] extends InlineConversion[T, U]:
    inline def apply(t: T): U = t.asInstanceOf[U]
  end ByCast
end InlineConversion
