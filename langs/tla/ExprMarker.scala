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

// PART 1

// HARD:
// - identifiers
// - opcalls
// - prefix operators
// - infix operators
// - postfix operators

// EASY:

// - records
// - "." operand
// - tuples

// - IF/THEN/ELSE
// - LET


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
          // Parse Number and String literals
          on(
            leftSibling(ExprTry) *>
              field(TLAReader.NumberLiteral)
              ~ trailing
          ).rewrite: lit =>
            splice(lang.Expr(lang.Expr.NumberLiteral().like(lit)))
          | on(
            leftSibling(ExprTry) *>
              field(TLAReader.StringLiteral)
              ~ trailing
          ).rewrite: lit =>
            splice(lang.Expr(lang.Expr.StringLiteral().like(lit)))
          // Braces Group is complete when all the elements are parsed
          | on(
              leftSibling(ExprTry) *>
                field(tok(TLAReader.BracesGroup) *>
                  children:
                    field(
                      repeatedSepBy(`,`)(
                        skip(ExprTry) 
                        ~ field(lang.Expr)
                        ~ trailing
                      )
                    )
                    ~ eof
                )
                ~ trailing
          ).rewrite: exprs =>
            splice(
              lang.Expr(
                lang.Expr.SetLiteral(exprs.iterator.map(_.unparent())),
              )
          )
          // If the braces group is not complete, mark the elements with ExprTry
          // Mark the first child in the first pass, and then mark the rest in the second
          // TODO: I wonder if this pattern is a bad idea
          | on(parent(leftSibling(ExprTry) *> tok(TLAReader.BracesGroup)) *>
              not(leftSibling(anyNode)) *>
              not(ExprTry) *> not(lang.Expr) *> anyNode
          ).rewrite: first =>
            splice(ExprTry(), first.unparent())
          | on(
              parent(leftSibling(ExprTry) *> tok(TLAReader.BracesGroup)) *>
                tok(`,`) <* rightSibling(not(ExprTry) *> not(lang.Expr))
            ).rewrite: comma =>
              splice(comma.unparent(), ExprTry())
          // CASE is complete when every branch is parsed.
          | on(
            leftSibling(ExprTry) *>
              skip(defns.CASE)
              ~ field(
                repeatedSepBy1(defns.`[]`)(
                  skip(ExprTry)
                    ~ field(lang.Expr)
                    ~ skip(TLAReader.`->`)
                    ~ skip(ExprTry)
                    ~ field(lang.Expr)
                    ~ trailing,
                ),
              )
              ~ field(
                optional(
                  skip(defns.OTHER)
                    ~ skip(TLAReader.`->`)
                    ~ skip(ExprTry)
                    ~ field(lang.Expr)
                    ~ eof,
                ),
              )
              ~ trailing,
          ).rewrite: (cases, other) =>
              splice(
                lang.Expr(
                  lang.Expr.Case(
                    lang.Expr.Case.Branches(
                      cases.iterator.map((pred, branch) =>
                        lang.Expr.Case.Branch(pred.unparent(), branch.unparent()),
                      ),
                    ),
                    lang.Expr.Case.Other(
                      other match
                        case None       => lang.Expr.Case.None()
                        case Some(expr) => expr.unparent()
                    ),
                  ),
                )
              )
          // If the CASE is not complete, insert ExprTry after [], ->, and OTHER
          //  as well as before the first case.
          | on(
            leftSibling(leftSibling(ExprTry) *> defns.CASE) *>
                field(not(lang.Expr) *> not(ExprTry) *> anyNode)
                ~ trailing,
          ).rewrite: first =>
            splice(ExprTry(), first.unparent())
          | on(
            (tok(defns.`[]`) | tok(TLAReader.`->`)) 
              <* rightSibling(not(lang.Expr) *> not(ExprTry))
          ).rewrite: split =>
            splice(split.unparent(), ExprTry())
          | on(
            leftSibling(defns.OTHER) *> 
              tok(TLAReader.`->`) 
                <* rightSibling(not(lang.Expr) *> not(ExprTry))
          ).rewrite: split =>
            splice(split.unparent(), ExprTry())
          // Parenthesis can be removed when its children are parsed
          | on(
            leftSibling(ExprTry) *> 
              tok(TLAReader.ParenthesesGroup) *>
                children(
                  skip(ExprTry)
                  ~ field(lang.Expr)
                  ~ eof
                )
          ).rewrite: expr =>
            splice(expr.unparent())
          // Mark the children of the parenthesis
          | on(parent(leftSibling(ExprTry) *> tok(TLAReader.ParenthesesGroup)) *>
              not(leftSibling(anyNode)) *>
              not(ExprTry) *> not(lang.Expr) *> anyNode
          ).rewrite: node =>
            splice(ExprTry(), node.unparent())
      *> pass(once = false, strategy = pass.bottomUp)
        .rules:
          // Expr has been parsed sucessfully, and ExprTry can be removed
          // I have to do it this way, otherwise ExprTry might get removed too early.
          on(
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



// | on(
//   skip(ExprTry)
//   ~ field(tok(lang.Expr) <* rightSibling(tok(`,`) | tok(`->`) | tok(defns.`[]`)))
//   ~ trailing
// ).rewrite: expr =>
//   splice(expr.unparent())