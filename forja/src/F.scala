package forja

import scala.annotation.publicInBinary

final class F[T] @publicInBinary private[forja] (@publicInBinary private[forja] val underlyingErased: Any) extends AnyVal

object F:
  //
end F
