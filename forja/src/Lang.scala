package forja

import scala.util.NotGiven
import scala.deriving.Mirror
import scala.annotation.publicInBinary
import scala.compiletime.asMatchable
import java.util.Objects
import scala.compiletime.Erased
import scala.compiletime.summonInline
import forja.util.TrivialMatch
import scala.quoted.Type
import forja.util.TupleMacros.*
import forja.util.ExprMacros.*
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.quotes
import scala.annotation.tailrec
import forja.LangMacros.*

trait Lang:
  final transparent inline given this.type = this
end Lang

object Lang:
  trait Extend[Base <: Lang](using val up: Base) extends Lang

  sealed abstract class NodeT[N <: Node] extends Erased:
    type T
  end NodeT

  object NodeT:
    final class Launder[N <: Node, T]
    object Launder:
      given instance: [N <: Node] => (N: N) => Launder[N, N.T] =
        new Launder
      end instance
    end Launder

    final class Aux[N <: Node, T0] extends NodeT[N]:
      type T = T0
    end Aux

    inline given instance
        : [N <: Node, T] => (inline ev: Launder[N, T]) => Aux[N, T] =
      new Aux
    end instance
  end NodeT

  sealed abstract class TNode[T] extends Erased:
    type N <: Node
  end TNode

  object TNode:
    final class Launder[T, N <: Node]

    final class Aux[T, N0 <: Node] extends TNode[T]:
      type N = N0
    end Aux

    inline given instance
        : [T, N <: Node] => (inline ev: Launder[T, N]) => Aux[T, N] =
      new Aux
    end instance
  end TNode

  final class NodeRetract[N <: Node] extends Erased

  object NodeRetract:
    final class Launder[N <: Node]

    object Launder:
      inline given instance: [N <: Node] => (N: N) => N.Retract => Launder[N] =
        new Launder
    end Launder

    inline given instance
        : [N <: Node] => (inline ev: Launder[N]) => NodeRetract[N] =
      new NodeRetract
    end instance
  end NodeRetract

  sealed abstract class NodeReplaceWith[N1 <: Node] extends Erased:
    type OtherNode <: Node
  end NodeReplaceWith

  object NodeReplaceWith:
    final class Aux[N <: Node, OtherNode0 <: Node] extends NodeReplaceWith[N]:
      type OtherNode = OtherNode0
    end Aux

    final class Launder[N <: Node, OtherNode <: Node]

    object Launder:
      inline given instance: [N <: Node, OtherNode <: Node] => (N: N)
        => N.ReplaceWith[OtherNode] => Launder[N, OtherNode] =
        new Launder
      end instance
    end Launder

    inline given instance: [N <: Node, OtherNode <: Node]
      => (inline ev: Launder[N, OtherNode]) => Aux[N, OtherNode] =
      new Aux
    end instance
  end NodeReplaceWith

  sealed abstract class Data:
    type Node <: Lang.Node
  end Data

  trait Node:
    node =>
    final transparent inline given node.type = node

    abstract class ReplaceWith[Replacement0 <: Node] extends Node.ReplaceWithAux[node.type]:
      final type Replacement = Replacement0
    end ReplaceWith

    abstract class Retract extends ReplaceWith[Nothing]

    type T <: Data { type Node = node.type }

    inline given TNode.Launder[T, this.type] = new TNode.Launder
  end Node

  object Node:
    sealed abstract class ReplaceWithAux[N <: Node]:
      type Replacement <: Node
    end ReplaceWithAux
  end Node

  trait Sum extends Node:
    sum =>
    type Case <: Node

    final into class T(val ordinal: Int, val t: Node#T) extends Data:
      type Node = sum.type
      // transparent inline def ex(using inline ng: NotGiven[ReplaceWith[?]])(using et: Sum.EffectiveType[sum.type]): et.T =
      //   t.asInstanceOf[et.T].runtimeChecked
      // end ex
    end T

    object T:
      // inline given broadConversion: [T <: Node#T] => Sum.BroadConversion[T, sum.T] = new Sum.BroadConversion

      // inline given identUpConversion: Sum.IdentUpConversion[T] = new Sum.IdentUpConversion
      // inline given ordinalUpConversion: [T <: Node#T, V <: Sum#T] => (ord: Sum.OrdinalOf[T, sum.T]) => (rec: Sum.UpConversion[sum.T, V]) => Sum.OrdinalUpConversion[T, sum.T, ord.Ord, sum.type, V, rec.type] =
      //   new Sum.OrdinalUpConversion(rec)
      // end ordinalUpConversion
    end T

    trait Extends extends Sum:
      final type Super = sum.type
    end Extends
  end Sum

  object Sum:
    sealed trait CaseList[S <: Sum] extends Erased:
      type Cases <: Tuple
    end CaseList

    object CaseList:
      final class Aux[S <: Sum, Cases0 <: Tuple] extends CaseList[S]:
        type Cases = Cases0
      end Aux

      transparent inline given instance: [S <: Sum] => CaseList.Aux[S, ? <: Tuple] =
        ${ instanceImpl[S] }
      end instance

      private[Sum] def instanceImpl[S <: Sum : Type](using Quotes): Expr[CaseList.Aux[S, ? <: Tuple]] =
        val prefix: List[Type[?]] = Type.of[S] match
          case '[type up <: Sum; Sum#Extends { type Super = up }] =>
            instanceImpl[up] match
              case '{ type prefix <: Tuple; $_ : CaseList.Aux[?, prefix] } =>
                Type.of[prefix].toList
            end match
          case '[S] => Nil
        end prefix

        Type.of[S] match
          case '[ S { type Case = cse } ] =>
            Expr.summonOrAbort[Mirror.SumOf[cse]] match
              case '{ type cases <: Tuple; $_ : Mirror.Sum { type MirroredElemTypes = cases }} =>
                val localCases = Type.of[cases]
                  .toList
                  .map:
                    case '[type n <: Node; n] =>
                      effectiveNodeImpl[n]
                  .filter:
                    case '[Nothing] => false
                    case _ => true
                end localCases
                (prefix ::: localCases).toTupleType match
                  case '[type casesFinal <: Tuple; casesFinal] =>
                    '{ new CaseList.Aux[S, casesFinal] }
                end match
            end match
        end match
      end instanceImpl
    end CaseList

    sealed trait CaseListT[T <: Sum#T] extends Erased:
      type Cases <: Tuple
    end CaseListT

    object CaseListT:
      final class Aux[T <: Sum#T, Cases0 <: Tuple] extends CaseListT[T]:
        type Cases = Cases0
      end Aux

      transparent inline given instance: [T <: Sum#T] => Aux[T, ? <: Tuple] =
        ${ instanceImpl[T] }
      end instance

      private def instanceImpl[T <: Sum#T : Type](using Quotes): Expr[Aux[T, ? <: Tuple]] =
        Type.of[T].tNode match
          case '[type n <: Sum; n] =>
            CaseList.instanceImpl[n] match
              case '{ $_ : CaseList.Aux[?, tpl] } =>
                Type.of[tpl] match
                  case '[type tpl <: Tuple; tpl] =>
                    Type.of[tpl]
                      .toList
                      .map:
                        case '[type cs <: Node; cs] =>
                          Type.of[cs].nodeT
                      .toTupleType
                      match
                        case '[type tplT <: Tuple; tplT] =>
                          '{ new CaseListT.Aux[T, tplT] }
                      end match
                end match
            end match
        end match
      end instanceImpl
    end CaseListT

    sealed abstract class OrdinalOf[T <: Node#T, U <: Sum#T] extends Erased:
      type Ord <: Int
    end OrdinalOf

    object OrdinalOf:
      final class Aux[T <: Node#T, U <: Sum#T, Ord0 <: Int] extends OrdinalOf[T, U]:
        type Ord = Ord0
      end Aux

      transparent inline given instance: [T <: Node#T, U <: Sum#T] => (csl: CaseListT[U]) => (inline ev: T <:< Tuple.Union[csl.Cases]) => OrdinalOf[T, U] =
        ${ instanceImpl[T, U, csl.Cases] }
      end instance

      private def instanceImpl[T <: Node#T : Type, U <: Sum#T : Type, Cases <: Tuple : Type](using Quotes): Expr[OrdinalOf[T, U]] =
        import quotes.reflect.*
        val ord = Type.of[Cases]
          .toList
          .indexWhere:
            case '[? <: T] => true
            case _ => false
        if ord == -1
        then
          report.errorAndAbort(s"Could not find ${TypeRepr.of[T].show} in ${TypeRepr.of[Cases].show}")
        end if
        ConstantType(IntConstant(ord)).asType match
          case '[type ord <: Int; ord] =>
            '{ new Aux[T, U, ord] }
        end match
      end instanceImpl
    end OrdinalOf

    final class BroadConversion[T <: Node#T, U <: Sum#T] extends Conversion[T, U], Erased:
      inline def apply(t: T): U =
        ${ BroadConversion.applyImpl[T, U]('{ t }) }
      end apply
    end BroadConversion

    sealed abstract class UpConversion[T <: Node#T, U <: Sum#T] extends Erased:
      inline def apply(t: T): U
    end UpConversion

    object UpConversion:
      opaque type Arg[T <: Node#T] = T
      opaque type Result[U <: Sum#T] = U

      object Arg:
        inline def apply[T <: Node#T](t: T): Arg[T] = t
      end Arg

      extension [U <: Sum#T](res: Result[U])
        inline def value: U = res
      end extension

      transparent inline given compute: [T <: Node#T, U <: Sum#T] => (arg: Arg[T]) => (conv: UpConversion[T, U]) => Result[U] =
        conv(arg)
      end compute
    end UpConversion

    final class IdentUpConversion[T <: Sum#T] extends UpConversion[T, T]:
      inline def apply(t: T): T = t
    end IdentUpConversion

    final class OrdinalUpConversion[T <: Node#T, U <: Sum#T, Ord <: Int, S <: Sum, V <: Sum#T, Rec <: UpConversion[U, V]](val rec: Rec) extends UpConversion[T, V]:
      inline def apply(t: T): V =
        val S: S = summonInline[S]
        rec(S.T(valueOf[Ord], t).asInstanceOf[U])
      end apply
    end OrdinalUpConversion

    // final class Ordinal[S, T <: Node#T](val i: Int) extends AnyVal

    // object Ordinal:
    //   sealed abstract class Search[Cases <: Tuple, T <: Node#T] extends Erased:
    //     type I <: Int
    //   end Search

    //   object Search:
    //     final class Aux[Cases <: Tuple, T <: Node#T, I0 <: Int] extends Search[Cases, T]:
    //       type I = I0
    //     end Aux

    //     inline given found: [T <: Node#T, Tl <: Tuple] => Aux[T *: Tl, T, 0] = new Aux

    //     inline given succ: [T <: Node#T, Hd <: Node#T, Tl <: Tuple] => (inline ng: NotGiven[T =:= Hd]) => (rec: Search[Tl, T]) => Aux[Hd *: Tl, T, rec.I + 1] = new Aux
    //   end Search

    //   inline given instance: [S <: Sum, T <: Node#T] => (tl: Sum.EffectiveTypeList[S]) => (tlp: Sum.EffectiveType.EffectiveTypeProjection[tl.Cases]) => (src: Search[tlp.T, T]) => (v: ValueOf[src.I]) => Ordinal[S, T] =
    //     Ordinal(v.value)
    //   end instance
    // end Ordinal

    // // Intentionally over-broad conversion so we see error messages from the summons inside it, not the generic inapplicable conversion message (that is, act like there is no conversion)
    // inline given subtypeConversionPoint: [T <: Node#T, U <: Sum#T, ET]
  //     => (inline ng: NotGiven[T =:= U]) => (tn: TNode[T]) => (un: TNode[U])
  //     => (unet: EffectiveType[un.N]) => (unet.T =:= ET)
  //     => SubtypeConversion[T, U] =
  //     // Not sure how useful this is, but it's a sanity check to ensure no one is trying to use a replaced Sum instead of its replacement.
  //     summonInline[NotGiven[NodeReplaceWith[un.N]]]
  //     // Extra type param ET is used to "encourage" the compiler to print the actual type, not a term reference to part of our inline elaboration,
  //     // in case this summon fails.
  //     summonInline[T <:< ET]
  //     new SubtypeConversion[T, U](
  //       sum = summonInline[un.N].asInstanceOf[Sum],
  //       ordinal = summonInline[Ordinal[un.N, T]].i,
  //     )
  //   end subtypeConversionPoint
  // end Sum

  trait Term[Members <: NamedTuple.AnyNamedTuple] extends Node:
    term: Singleton =>

    // private[Lang] given ordinal: Term.Ordinal[term.type] = deferred
    
    final class T @publicInBinary private[Lang] (
        private[Lang] val repr: Any,
    ) extends Data:
      type Node = term.type
      override def toString(): String = s"${term.getClass().getName()}$repr"
      override def equals(obj: Any): Boolean =
        obj.asMatchable match
          case other: T => repr == other.repr
        end match
      end equals
      override def hashCode(): Int =
        Objects.hash(term, repr)
      end hashCode
    end T

    inline def apply(using repr: Term.Repr[Members])(using
      inline ng: NotGiven[Node.ReplaceWithAux[term.type]],
      inline ev: repr.R <:< Unit,
    )(): T =
      new T(())
    end apply

    inline def apply(using repr: Term.Repr[Members])(using
        inline ng: NotGiven[Node.ReplaceWithAux[term.type]],
        inline ng2: NotGiven[repr.R <:< Unit],
    )(t: repr.R): T =
      new T(t)
    end apply

    // TODO: 

    // Patterns and inline do not go well together. Making this inline will create and then call
    // a lambda with the inline body, which is strictly worse than just calling the method.
    // This issue only happens in patterns; the apply above translates to new T properly.
    def unapply(using repr: Term.Repr[Members])(t: T)(using NotGiven[Node.ReplaceWithAux[term.type]]): Term.UnapplyR[repr.R] =
      // TODO: exactly this but at compile time, or I suspect TrivialMatch will be boxed
      val res =
        if t.repr.isInstanceOf[Unit]
        then true
        else TrivialMatch(t.repr.asInstanceOf[repr.R])
      end res
      res.asInstanceOf[Term.UnapplyR[repr.R]]
    end unapply
  end Term

  object Term:
    type UnapplyR[R] = R match
      case Unit => true
      case _ => TrivialMatch[R]
    end UnapplyR

    sealed abstract class Repr[Members <: NamedTuple.AnyNamedTuple] extends Erased:
      type R
    end Repr

    object Repr:
      final class Aux[Members <: NamedTuple.AnyNamedTuple, R0] extends Repr[Members]:
        type R = R0
      end Aux

      given instance0: Aux[NamedTuple.Empty, Unit] = new Aux
      given instance1: [L <: String, T] => (et: EffectiveType[T]) => Aux[NamedTuple.NamedTuple[Tuple1[L], Tuple1[T]], et.To] = new Aux
      given instanceN: [Members <: NamedTuple.AnyNamedTuple] => (et: EffectiveType[Members]) => Aux[Members, et.To] = new Aux
    end Repr

    opaque type Ordinal[Trm <: Term[?]] = Int

    object Ordinal:
      transparent inline given instance: [Trm <: Term[?]] => Ordinal[Trm] =
        ${ instanceImpl[Trm] }
      end instance

      private def instanceImpl[Trm <: Term[?] : Type](using Quotes): Expr[Ordinal[Trm]] =
        import quotes.reflect.*
        report.errorAndAbort(TypeRepr.of[Trm].show)
        Type.of[Trm] match
          case '[type cs <: Sum#Case; cs] =>
            report.errorAndAbort(TypeRepr.of[cs].show)
          case _ => '{ 0 : Ordinal[Trm] }
        end match
      end instanceImpl
    end Ordinal
  end Term

  trait Atom extends Term[NamedTuple.Empty]:
    atom: Singleton =>
  end Atom

  sealed abstract class EffectiveType[From] extends Erased:
    type To
  end EffectiveType

  object EffectiveType:
    sealed class Aux[From, To0] extends EffectiveType[From]:
      type To = To0
    end Aux

    final class Ident[T] extends Aux[T, T]
  end EffectiveType

  transparent inline given effectiveNodeT: [T <: Node#T] => EffectiveType.Aux[T, ? <: Node#T] =
    ${ effectiveNodeTImpl[T] }
  end effectiveNodeT

  @tailrec
  private def effectiveNodeImpl[N <: Node : Type](using Quotes): Type[? <: Node] =
    Expr.summon[Node.ReplaceWithAux[N]].runtimeChecked match
      case None => Type.of[N]
      case Some('{ type n2 <: Node; $_ : Node.ReplaceWithAux[?] { type Replacement = n2 } }) =>
        effectiveNodeImpl[n2]
    end match
  end effectiveNodeImpl

  private def effectiveNodeTImpl[T <: Node#T : Type](using Quotes): Expr[EffectiveType.Aux[T, ? <: Node#T]] =
    Type.of[T].tNode match
      case '[type n <: Node; n] =>
        effectiveNodeImpl[n] match
          case '[type n2 <: Node; n2] =>
            Type.of[n2].nodeT match
              case '[type t2 <: Node#T; t2] =>
                '{ new EffectiveType.Aux[T, t2] }
            end match
        end match
    end match
  end effectiveNodeTImpl

  transparent inline given effectiveTuple: [T <: Tuple] => EffectiveType.Aux[T, ? <: Tuple] =
    ${ effectiveTupleImpl[T] }
  end effectiveTuple
  
  private def effectiveTupleImpl[T <: Tuple : Type](using Quotes): Expr[EffectiveType.Aux[T, ? <: Tuple]] =
    Type.of[T]
      .toList
      .map:
        case '[elem] =>
          Expr.summonOrAbort[EffectiveType[elem]] match
            case '{ $_ : EffectiveType.Aux[?, elemRes] } =>
              Type.of[elemRes]
          end match
      .toTupleType
      match
        case '[type res <: Tuple; res] =>
          '{ new EffectiveType.Aux[T, res] }
      end match
  end effectiveTupleImpl
  
  transparent inline given effectiveNamedTuple: [T <: NamedTuple.AnyNamedTuple]
    => EffectiveType.Aux[T, ? <: NamedTuple.AnyNamedTuple] =
    ${ effectiveNamedTupleImpl[T] }
  end effectiveNamedTuple

  private def effectiveNamedTupleImpl[T <: NamedTuple.AnyNamedTuple : Type](using Quotes): Expr[EffectiveType.Aux[T, ? <: NamedTuple.AnyNamedTuple]] =
    effectiveTupleImpl[NamedTuple.DropNames[T]] match
      case '{ type tplRes <: Tuple; $_ : EffectiveType.Aux[?, tplRes] } =>
        '{ new EffectiveType.Aux[T, NamedTuple.NamedTuple[NamedTuple.Names[T], tplRes]] }
    end match
  end effectiveNamedTupleImpl

  inline given EffectiveType.Ident[Boolean] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Byte] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Char] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Short] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Int] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Long] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Float] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Double] = new EffectiveType.Ident

  inline given EffectiveType.Ident[String] = new EffectiveType.Ident
  inline given EffectiveType.Ident[Unit] = new EffectiveType.Ident

  inline given [T] => (et: EffectiveType[T])
    => EffectiveType.Aux[Option[T], Option[et.To]] = new EffectiveType.Aux
  inline given [T] => (et: EffectiveType[T])
    => EffectiveType.Aux[List[T], List[et.To]] = new EffectiveType.Aux
end Lang
