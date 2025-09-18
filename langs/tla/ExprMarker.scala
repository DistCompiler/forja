// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.langs.tla

import cats.syntax.all.given

import forja.*
import forja.dsl.*
import forja.wf.Wellformed

import TLAReader.*

object ExprMarker extends PassSeq:

  lazy val passes = List(
    buildExpressions,
  )

  object ExprTry extends Token
  def inputWellformed: Wellformed = TLAParser.outputWellformed

  object buildExpressions extends Pass:
    val wellformed = prevWellformed.makeDerived:
      val removedCases = Seq(
        TLAReader.StringLiteral,
        TLAReader.NumberLiteral,
        TLAReader.TupleGroup,
        // TODO: remove cases
      )
      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(removedCases*)
        tok.addCases(lang.Expr)

      lang.Expr.deleteShape()
      lang.Expr.importFrom(lang.wf)
      lang.Expr.addCases(lang.Expr)


    val rules = 
      pass(once = false, strategy = pass.bottomUp)
        .rules:
          // Number and String Literals can be parsed directly
          on(
            TLAReader.NumberLiteral,
          ).rewrite: lit =>
            splice(lang.Expr(lang.Expr.NumberLiteral().like(lit)))
          | on(
            TLAReader.StringLiteral,
          ).rewrite: lit =>
            splice(lang.Expr(lang.Expr.StringLiteral().like(lit)))
          // Isolated Alpha can be parsed
          | on(
            skip(ExprTry)
            ~ field(TLAReader.Alpha)
            ~ eof
          ).rewrite: id =>
            splice(
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id().like(id),
                  lang.Expr.OpCall.Params(),
                )
              )
            )
          // Braces Group is complete when the elements are parsed
          | on(
              skip(ExprTry)
              ~ field(
                tok(TLAReader.BracesGroup) *>
                  children:
                    field(repeatedSepBy(`,`)(lang.Expr))
                    ~ eof
              )
              ~ trailing
          ).rewrite: exprs =>
            splice(
              lang.Expr(
                lang.Expr.SetLiteral(exprs.iterator.map(_.unparent())),
              )
            ) 
          // If the braces group is not complete, mark the next element as ExprTry
          // | on(
          //     parent(tok(TLAReader.BracesGroup) *> rightSibling(ExprTry)) *>
          //     field(lang.Expr)
          //     ~ field(`,` *> rightSibling(not(ExprTry)))
          //   ).rewrite: (expr, comma) =>
          //     splice(expr, comma, ExprTry())
          // Expr has been parsed and ExprTry can be removed
          | on(
            skip(ExprTry)
            ~ field(lang.Expr)
            ~ eof
          ).rewrite: expr =>
            splice(expr.unparent())
  end buildExpressions

  object removeNestedExpr extends Pass:
    val wellformed = prevWellformed.makeDerived:
      lang.Expr.removeCases(lang.Expr)

    val rules = 
      pass(once = true, strategy = pass.bottomUp)
        .rules:
          on(
            tok(lang.Expr) *>
              onlyChild(lang.Expr),
          ).rewrite: child =>
            splice(
              child.unparent(),
            )
  end removeNestedExpr
