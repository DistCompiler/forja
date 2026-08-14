package forja

import scala.annotation.publicInBinary
import scala.quoted.Type
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.quotes
import forja.Lang.Node
import forja.Lang.Sum

private[forja] object LangMacros:
  object BroadConversion:
    @publicInBinary
    def applyImpl[T <: Node#T : Type, V <: Sum#T : Type](t: Expr[T])(using Quotes): Expr[V] =
      import quotes.reflect.*
      Type.of[V].tNode match
        case '[type n <: Sum; n] =>
          Expr.summon[n] match
            case Some(_) =>
              // Expr.summonOrAbort[Sum.OrdinalOf[T, ?]] match
              //   case '{ type u <: Sum#T; type ord <: Int; $_ : Sum.OrdinalOf.Aux[?, u, ord] } =>
              //     ???
              report.errorAndAbort(s"err ${TypeRepr.of[T].show}")
            case None =>
              // report.info("This is a compile-time only Conversion apply stub")
              '{ throw RuntimeException("invoked runtime stub for BroadConversion") }
          end match
      end match
    end applyImpl
  end BroadConversion

  // I tried doing this with type matching , but it got me stuck on a seemingly
  // unavoidable compiler error, where it could not tell a term and its exported counterpart
  // were the same. Best use built-in selection rather than try to be slick with patterns.
  extension (t: Type[? <: Node#T])
    private[forja] def tNode(using Quotes): Type[? <: Node] =
      import quotes.reflect.*
      val sym = TypeRepr.of[Node#T].typeSymbol.declaredType("Node").head
      t match
        case '[t] =>
          TypeRepr.of[t].select(sym).asType match
            case '[type n <: Node; n] =>
              Type.of[n]
          end match
      end match
    end tNode
  end extension

  extension (n: Type[? <: Node])
    private[forja] def nodeT(using Quotes): Type[? <: Node#T] =
      import quotes.reflect.*
      val sym = TypeRepr.of[Node].typeSymbol.declaredType("T").head
      n match
        case '[n] =>
          TypeRepr.of[n].select(sym).asType match
            case '[type t <: Node#T; t] =>
              Type.of[t]
          end match
      end match
    end nodeT
  end extension
end LangMacros
