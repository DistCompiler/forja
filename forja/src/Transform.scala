package forja

// import forja.Lang.Node
// import forja.Lang.TNode
// import forja.Lang.EffectiveType
// import forja.Lang.Term
// import forja.Lang.Sum
// import scala.compiletime.summonAll
// import scala.quoted.Quotes
// import scala.quoted.Expr
// import scala.quoted.Type
// import scala.quoted.quotes
// import forja.Lang.NodeT
// import scala.compiletime.Erased
// import scala.quoted.Exprs

sealed abstract class Transform[From, To] extends Function1[From, To]

object Transform:
  // abstract class Auto[From, To]:
  //   def apply(from: From): To
  // end Auto

  // abstract class User[From, To] extends Transform[From, To]

  // sealed abstract class AutoTransformImpl[From, To] extends Transform[From, To]

  // given autoLazy: [From, To] => (auto: =>Auto[From, To]) => AutoTransformImpl[From, To]:
  //   def apply(v1: From): To = auto.apply(v1)
  // end autoLazy

  // abstract class Ident[T] extends Transform[T, T]:
  //   inline def apply(from: T): T = from
  // end Ident

  // given Ident[Boolean]
  // given Ident[Byte]
  // given Ident[Char]
  // given Ident[Short]
  // given Ident[Int]
  // given Ident[Long]
  // given Ident[Float]
  // given Ident[Double]
  // given Ident[String]
  // given Ident[Unit]

  // given transformOption: [T, U] => (transformElem: Transform[T, U]) => Auto[Option[T], Option[U]]:
  //   def apply(v1: Option[T]): Option[U] = v1.map(transformElem)
  // end transformOption

  // given transformList: [T, U] => (transformElem: Transform[T, U]) => Auto[List[T], List[U]]:
  //   def apply(v1: List[T]): List[U] = v1.map(transformElem)
  // end transformList

  // private final class TransformSum[From <: Sum#T, To <: Node#T](transformers: Tuple) extends Auto[From, To]:
  //   def apply(from: From): To =
  //     transformers
  //       .productElement(from.ordinal)
  //       .asInstanceOf[Transform[From, To]]
  //       .apply(from)
  //   end apply
  // end TransformSum

  // // inline given transformSum: [From <: Sum#T, To <: Node#T] => (fromN: TNode[From]) => (etl: Lang.Sum.EffectiveTypeList[fromN.N]) => (inst: SumTransformInstances[etl.Cases, To]) => Auto[From, To] =
  // //   ${ transformSumImpl[From, To, inst.Instances] }
  // // end transformSum

  // sealed abstract class SumTransformInstances[Cases <: Tuple, To <: Node#T] extends Erased:
  //   type Instances <: Tuple
  // end SumTransformInstances

  // object SumTransformInstances:
  //   final class Aux[Cases <: Tuple, To <: Node#T, Instances0 <: Tuple] extends SumTransformInstances[Cases, To]:
  //     type Instances = Instances0
  //   end Aux

  //   given empty: [To <: Node#T] => Aux[EmptyTuple, To, EmptyTuple] = new Aux
  //   given cons: [Hd <: Sum, Tl <: Tuple, To <: Node#T] => (nt: NodeT[Hd]) => (rec: SumTransformInstances[Tl, To]) => Aux[Hd *: Tl, To, Transform[nt.T, To] *: rec.Instances] = new Aux
  // end SumTransformInstances

  // private def transformSumImpl[From <: Sum#T : Type, To <: Node#T : Type, Instances <: Tuple : Type](using Quotes): Expr[Auto[From, To]] =
  //   '{
  //     new Auto[From, To]:
  //       def apply(from: From): To =
  //         val ord = from.ordinal
  //         ${
  //           import quotes.reflect.*
  //           report.errorAndAbort(TypeRepr.of[Instances].show)
  //           ???
  //         }
  //       end apply
  //     end new
  //   }
  // end transformSumImpl

  // inline given transformTerm: [Members <: NamedTuple.AnyNamedTuple, From <: Term[Members]#T, To <: Node#T] => (toN: TNode[To]) => (et: EffectiveType[toN.N]) => Auto[From, To] =
  //   ???
  // end transformTerm

  // TODO: transformUp, specifically the act of finding an ordinal and moving 1 layer of sums upward.
  // needs a way to query what types are acceptable in a given position (regardless of how we convert to them)
  // ... this is the same mechanism as we need for correct auto-conversion support. Basically check + transform.
  
  // ... it would be so much easier if I could just codegen little conversion fragments; can I fool the apply override in Conversion?

  // also we need to be able to fish for "compatible" terms somehow. Possibly by shadowing Term / Sum
end Transform
