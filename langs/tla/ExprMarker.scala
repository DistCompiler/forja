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



// TODO:

// - records
// - "." operand

// - IF/THEN/ELSE
// - LET


  object ExprTry extends Token
  def inputWellformed: Wellformed = TLAParser.outputWellformed

  def parsedChildrenSepBy(
    parent: Token, split: SeqPattern[?]
  ): SeqPattern[List[Node]] =
    leftSibling(ExprTry) *>
      tok(parent) *>
        children:
          field(
            repeatedSepBy(split)(
              skip(ExprTry) 
              ~ field(lang.Expr)
              ~ trailing
            )
          )
          ~ eof
  end parsedChildrenSepBy

  // TODO: I wonder if this pattern is a bad idea
  def firstUnmarkedChildStart(
    paren: Token
  ): SeqPattern[Node] = 
    parent(leftSibling(ExprTry) *> paren) *>
      not(leftSibling(anyNode)) *>
      not(ExprTry) *> not(lang.Expr) *> anyNode
  end firstUnmarkedChildStart
  
  def unMarkedChildSplit(
    paren: Token,
    split: Token,
  ): SeqPattern[Node] =
    parent(leftSibling(ExprTry) *> paren) *>
       possibleExprTryToRight(split)
  end unMarkedChildSplit

  def rightSiblingNotExprTry(): SeqPattern[Unit] =
    rightSibling(not(lang.Expr) *> not(ExprTry))
  end rightSiblingNotExprTry

  def possibleExprTryToRight(
    node: SeqPattern[Node]
  ): SeqPattern[Node] =
      node <* rightSiblingNotExprTry()
  end possibleExprTryToRight
    

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
          // TupleLiteral is complete when all the elements are parsed
          | on(
              parsedChildrenSepBy(TLAReader.TupleGroup, `,`)
          ).rewrite: exprs =>
            splice(
              lang.Expr(
                lang.Expr.TupleLiteral(exprs.iterator.map(_.unparent())),
              )
          )
           // If the TupleLiteral not complete, mark the elements with ExprTry
          // Mark the first child in the first pass, and then mark the rest in the second
          | on(
            firstUnmarkedChildStart(TLAReader.TupleGroup)
          ).rewrite: first =>
            splice(ExprTry(), first.unparent())
          | on(
            unMarkedChildSplit(TLAReader.TupleGroup, `,`)
          ).rewrite: split =>
              splice(split.unparent(), ExprTry())
          // SetLiteral is complete when all the elements are parsed
          | on(
              parsedChildrenSepBy(TLAReader.BracesGroup, `,`)
          ).rewrite: exprs =>
            splice(
              lang.Expr(
                lang.Expr.SetLiteral(exprs.iterator.map(_.unparent())),
              )
          )
          // If the SetLiteral is not complete, mark the elements with ExprTry
          // Mark the first child in the first pass, and then mark the rest in the second
          | on(
            firstUnmarkedChildStart(TLAReader.BracesGroup)
          ).rewrite: first =>
            splice(ExprTry(), first.unparent())
          | on(
            unMarkedChildSplit(TLAReader.BracesGroup, `,`)
          ).rewrite: split =>
              splice(split.unparent(), ExprTry())
          // RecordLiteral is complete when all the elements are parsed
          | on(
            leftSibling(ExprTry) *> tok(TLAReader.SqBracketsGroup) *>
              children:
                field(
                  repeatedSepBy1(`,`)(
                    field(TLAReader.Alpha)
                    ~ skip(TLAReader.`|->`)
                    ~ skip(ExprTry)
                    ~ field(lang.Expr)
                    ~ trailing
                  )
                )
                ~ eof
          ).rewrite: records =>
            splice(
              lang.Expr(
                lang.Expr.RecordLiteral(
                  records.iterator.map(
                    (id, expr) =>
                        lang.Expr.RecordLiteral.Field(
                          lang.Id().like(id.unparent()),
                          expr.unparent(),
                        )
                  )
                )
              )
            )
          // If the RecordLiteral is not complete, place ExprTry after each |->
          | on (
            unMarkedChildSplit(TLAReader.SqBracketsGroup, `|->`)
          ).rewrite: split =>
            splice(split.unparent(), ExprTry())   
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
            possibleExprTryToRight(leftSibling(ExprTry) *> defns.CASE)
          ).rewrite: c =>
            splice(c.unparent(), ExprTry())
          | on(
            possibleExprTryToRight((tok(defns.`[]`) | tok(TLAReader.`->`)))
          ).rewrite: split =>
            splice(split.unparent(), ExprTry())
          | on(
            possibleExprTryToRight(leftSibling(defns.OTHER) *> tok(TLAReader.`->`))
          ).rewrite: split =>
            splice(split.unparent(), ExprTry())
          // Parentheses can be removed when its children are parsed
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
          // Mark the children of the parentheses
          | on(
            firstUnmarkedChildStart(TLAReader.ParenthesesGroup)
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