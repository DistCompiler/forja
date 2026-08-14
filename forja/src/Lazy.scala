package forja

into sealed abstract class Lazy[+T]:
  def isError: Boolean

  def toPartial: Lazy.Partial[T]
end Lazy

object Lazy:
  sealed abstract class Partial[+T] extends Lazy[T]

  // final class Pure[T](value: T) extends Partial[T]

  // // final class Term[Members <: AnyNamedTuple](term: Lang.Term[Members], data: Tuple) extends Partial[T]

  // private final class Deferred[T, U](data: Lazy[T], action: Partial[T] => Lazy[U]) extends Lazy[U]:
  //   lazy val result = ??? // action(data)
  // end Deferred

  // private final class Error[T, U](src: T, err: String) extends Lazy[U]
end Lazy
