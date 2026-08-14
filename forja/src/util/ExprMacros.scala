package forja.util

import scala.quoted.Quotes
import scala.quoted.quotes
import scala.quoted.Type
import scala.quoted.Expr

object ExprMacros:
  extension (_E : Expr.type)(using Quotes)
    def summonOrAbort[T : Type]: Expr[T] =
      import quotes.reflect.*
      Implicits.search(TypeRepr.of[T]) match
        case succ: ImplicitSearchSuccess =>
          succ.tree.asExprOf[T]
        case fail: ImplicitSearchFailure =>
          report.errorAndAbort(s"given search for ${TypeRepr.of[T].show} failed.\n${fail.explanation}")
      end match
    end summonOrAbort
  end extension
end ExprMacros
