package forja.util

import scala.compiletime.Erased
import scala.deriving.Mirror

sealed abstract class ErasedMirroredElemTypes[T] extends Erased:
  type MirroredElemTypes <: Tuple
end ErasedMirroredElemTypes

object ErasedMirroredElemTypes:
  final class Aux[T, MirroredElemTypes0 <: Tuple] extends ErasedMirroredElemTypes[T]:
    type MirroredElemTypes = MirroredElemTypes0
  end Aux

  final class Launder[T, MirroredElemTypes <: Tuple]
  
  object Launder:
    given instance: [T] => (mirror: Mirror.Of[T]) => Launder[T, mirror.MirroredElemTypes] = new Launder
  end Launder

  inline given instance: [T, MirroredElemTypes <: Tuple] => (inline ev: Launder[T, MirroredElemTypes]) => Aux[T, MirroredElemTypes] = new Aux
end ErasedMirroredElemTypes
