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
import TLAParser.{rawExpression, rawConjunction}

object ExprParser extends PassSeq:
  object TmpInfixGroup extends Token
  object TmpUnaryGroup extends Token

  def inputWellformed: Wellformed = TLAParser.outputWellformed
  // TODO: make private
  def highPredInfixInfix(
      op: defns.InfixOperator,
  ): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]] =
    field(
      lang.Expr.withChildren(
        field(
          TmpInfixGroup.withChildren(
            field(
              tok(defns.InfixOperator.instances*)
                .filter(op2 =>
                  op2.token match
                    case op2Token: defns.InfixOperator =>
                      op.highPrecedence > op2Token.highPrecedence,
                ),
            )
              ~ field(lang.Expr)
              ~ field(lang.Expr)
              ~ eof,
          ),
        )
          ~ eof,
      ),
    )
  def highPredInfixUnary(
      op: defns.InfixOperator,
  ): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node)]]] =
    field(
      lang.Expr.withChildren(
        field(
          TmpUnaryGroup.withChildren(
            field(
              tok(defns.PrefixOperator.instances*)
                .filter(op2 =>
                  op2.token match
                    case op2Token: defns.PrefixOperator =>
                      op.highPrecedence > op2Token.highPrecedence
                    case op2Token: defns.PostfixOperator =>
                      op.highPrecedence > op2Token.precedence,
                ),
            )
              ~ field(lang.Expr)
              ~ eof,
          ),
        )
          ~ eof,
      ),
    )
  def highPredUnaryInfix(
      op: defns.Operator,
  ): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]] =
    field(
      lang.Expr.withChildren(
        field(
          TmpInfixGroup.withChildren(
            field(
              tok(defns.InfixOperator.instances*)
                .filter(op2 =>
                  op2.token match
                    case op2Token: defns.InfixOperator =>
                      op.highPrecedence > op2Token.highPrecedence,
                ),
            )
              ~ field(lang.Expr)
              ~ field(lang.Expr)
              ~ eof,
          ),
        )
          ~ eof,
      ),
    )

  def badPredInfixInfix(
      op: defns.InfixOperator,
  ): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]] =
    field(
      lang.Expr.withChildren(
        field(
          TmpInfixGroup.withChildren(
            field(
              tok(defns.InfixOperator.instances*)
                .filter(op2 =>
                  op2.token match
                    case op2Token: defns.InfixOperator =>
                      !((op.highPrecedence > op2Token.highPrecedence)
                        || (op.lowPrecedence < op2Token.lowPrecedence)
                        || ((op == op2Token) && op.isAssociative)),
                ),
            )
              ~ field(lang.Expr)
              ~ field(lang.Expr)
              ~ eof,
          ),
        )
          ~ eof,
      ),
    )
  def badPredInfixUnary(
      op: defns.InfixOperator,
  ): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node)]]] =
    field(
      lang.Expr.withChildren(
        field(
          TmpUnaryGroup.withChildren(
            field(
              tok(defns.PrefixOperator.instances*)
                .filter(op2 =>
                  op2.token match
                    case op2Token: defns.PrefixOperator =>
                      !((op.highPrecedence > op2Token.highPrecedence)
                        || (op.lowPrecedence < op2Token.lowPrecedence))
                    case op2Token: defns.PostfixOperator =>
                      !((op.highPrecedence > op2Token.precedence)
                        || (op.lowPrecedence < op2Token.precedence)),
                ),
            )
              ~ field(lang.Expr)
              ~ eof,
          ),
        )
          ~ eof,
      ),
    )
  def badPredUnaryInfix(
      op: defns.Operator,
  ): SeqPattern[SeqPattern.Fields[Tuple1[(Node, Node, Node)]]] =
    field(
      lang.Expr.withChildren(
        field(
          TmpInfixGroup.withChildren(
            field(
              tok(defns.InfixOperator.instances*)
                .filter(op2 =>
                  op2.token match
                    case op2Token: defns.InfixOperator =>
                      !((op.highPrecedence > op2Token.highPrecedence)
                        || (op.lowPrecedence < op2Token.lowPrecedence)),
                ),
            )
              ~ field(lang.Expr)
              ~ field(lang.Expr)
              ~ eof,
          ),
        )
          ~ eof,
      ),
    )
  def matchQuantifierId(): SeqPattern[(Node, TLAParser.RawExpression)] =
    parent(lang.Expr) *>
      field(TLAReader.Alpha)
      ~ skip(defns.`\\in`)
      ~ field(rawExpression)
      ~ trailing
  def matchQuantifierIds(): SeqPattern[(List[Node], TLAParser.RawExpression)] =
    parent(lang.Expr) *>
      field(
        tok(TLAReader.TupleGroup).withChildren(
          field(repeatedSepBy(`,`)(tok(TLAReader.Alpha)))
            ~ eof,
        ),
      )
      ~ skip(defns.`\\in`)
      ~ field(rawExpression)
      ~ trailing

  // replace all lang.Expr(contents...) with
  // lang.Expr(lang.ExprTry, contents...)

  val buildExpressions = passDef:
    wellformed := prevWellformed.makeDerived:
      val removedCases = Seq(
        TLAReader.StringLiteral,
        TLAReader.NumberLiteral,
        TLAReader.TupleGroup,
        // TODO: remove cases
      )
      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(removedCases*)
        tok.addCases(lang.Expr)

      TmpInfixGroup ::= fields(
        choice(defns.InfixOperator.instances*),
        lang.Expr,
        lang.Expr,
      )
      TmpUnaryGroup ::= fields(
        choice(
          (defns.PrefixOperator.instances ++ defns.PostfixOperator.instances)*,
        ),
        lang.Expr,
      )

      lang.Expr.deleteShape()
      lang.Expr.importFrom(lang.wf)
      lang.Expr.addCases(lang.Expr, TmpInfixGroup, TmpUnaryGroup)

    // TODO: assign the correct source with .like()

    pass(once = false, strategy = pass.bottomUp) // conjunction alignment
      .rules:
        on(
          parent(lang.Expr) *>
            (field(tok(defns./\))
              ~ field(rawConjunction(1))
              ~ field(tok(defns./\))
              ~ field(rawConjunction(1))
              ~ eof).filter(things =>
              things match
                case (and1: Node, r1, and2: Node, r2) =>
                  val s1 = and1.sourceRange
                  val s2 = and2.sourceRange
                  /* println("\nCol 1: " +
                   * s1.source.lines.lineColAtOffset(s1.offset)) */
                  /* println("\nCol 2: " +
                   * s2.source.lines.lineColAtOffset(s2.offset)) */
                  s1.source.lines
                    .lineColAtOffset(s1.offset)
                    ._2 == s2.source.lines.lineColAtOffset(s2.offset)._2,
            ),
        ).rewrite: (and1, r1, and2, r2) =>
          splice(
            and1.unparent(),
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(and2.unparent()),
                lang.Expr.OpCall.Params(
                  r1.mkNode,
                  r2.mkNode,
                ),
              ),
            ),
          )
    *> pass(
      once = false,
      strategy = pass.bottomUp,
    ) // remove leading /\, remove paren
      .rules:
        // on(
        //   field(lang.Expr.withChildren(
        //     skip(defns./\)
        //     ~ field(lang.Expr)
        //     ~ eof
        //   ))
        //   ~ eof
        // ).rewrite: expr =>
        //    splice(expr.unparent())
        on(
          field(
            lang.Expr.withChildren(
              skip(defns./\)
                ~ field(rawExpression)
                ~ eof,
            ),
          )
            ~ eof,
        ).rewrite: expr =>
          splice(expr.mkNode)
    *> pass(
      once = false,
      strategy = pass.bottomUp,
    ) // resolve quantifiers/opCall
      .rules:
        on(
          parent(lang.Expr) *>
            field(TLAReader.Alpha)
            ~ field(
              TLAReader.ParenthesesGroup.withChildren(
                field(repeatedSepBy(`,`)(rawExpression))
                  ~ eof,
              ),
            )
            ~ trailing,
        ).rewrite: (fun, args) =>
          splice(
            lang.Expr(
              lang.Expr.OpCall(
                lang.Id().like(fun),
                lang.Expr.OpCall.Params(
                  args.iterator.map(_.mkNode),
                ),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\E"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierId()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (qBounds, expr) =>
          splice(
            lang.Expr(
              lang.Expr.Exists(
                lang.QuantifierBounds(
                  qBounds.iterator.map((id, qExpr) =>
                    lang.QuantifierBound(
                      lang.Id().like(id),
                      qExpr.mkNode,
                    ),
                  ),
                ),
                expr.mkNode,
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\E"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierIds()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (qBounds, expr) =>
          splice(
            lang.Expr(
              lang.Expr.Exists(
                lang.QuantifierBounds(
                  qBounds.iterator.map((ids, qExpr) =>
                    lang.QuantifierBound(
                      lang.Ids(
                        ids.iterator.map(id => lang.Id().like(id)),
                      ),
                      qExpr.mkNode,
                    ),
                  ),
                ),
                expr.mkNode,
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\A"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierId()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (qBounds, expr) =>
          splice(
            lang.Expr(
              lang.Expr.Forall(
                lang.QuantifierBounds(
                  qBounds.iterator.map((id, qExpr) =>
                    lang.QuantifierBound(
                      lang.Id().like(id),
                      qExpr.mkNode,
                    ),
                  ),
                ),
                expr.mkNode,
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            skip(tok(TLAReader.LaTexLike).src("\\A"))
            ~ field(repeatedSepBy1(`,`)(matchQuantifierIds()))
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (qBounds, expr) =>
          splice(
            lang.Expr(
              lang.Expr.Forall(
                lang.QuantifierBounds(
                  qBounds.iterator.map((ids, qExpr) =>
                    lang.QuantifierBound(
                      lang.Ids(
                        ids.iterator.map(id => lang.Id().like(id)),
                      ),
                      qExpr.mkNode,
                    ),
                  ),
                ),
                expr.mkNode,
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            skip(tok(defns.CHOOSE))
            ~ field(matchQuantifierId())
            ~ skip(TLAReader.`:`)
            ~ field(rawExpression)
            ~ trailing,
        ).rewrite: (qBound, expr) =>
          qBound match
            case (id, qExpr) =>
              splice(
                lang.Expr(
                  lang.Expr.Choose(
                    lang.QuantifierBound(
                      lang.Id().like(id),
                      qExpr.mkNode,
                    ),
                    expr.mkNode,
                  ),
                ),
              )
    // TODO: tuple qbound
    //       id nil
    //       tuple nil
    *> pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(lang.Expr) *>
            TLAReader.Alpha,
        ).rewrite: name =>
          splice(
            lang.Expr(
              lang.Id().like(name),
            ),
          )
        | on(
          parent(lang.Expr) *>
            TLAReader.NumberLiteral,
        ).rewrite: lit =>
          splice(lang.Expr(lang.Expr.NumberLiteral().like(lit)))
        | on(
          parent(lang.Expr) *>
            TLAReader.StringLiteral,
        ).rewrite: lit =>
          splice(lang.Expr(lang.Expr.StringLiteral().like(lit)))
        | on(
          parent(lang.Expr) *>
            tok(TLAReader.BracesGroup) *>
            children:
              field(repeatedSepBy(`,`)(rawExpression))
                ~ eof,
        ).rewrite: exprs =>
          splice(
            lang.Expr(
              lang.Expr.SetLiteral(exprs.iterator.map(_.mkNode)),
            ),
          )
        | on(
          parent(lang.Expr) *>
            tok(TLAReader.TupleGroup).product(
              children(
                field(repeatedSepBy(`,`)(rawExpression))
                  ~ eof,
              ),
            ),
        ).rewrite: (lit, elems) =>
          splice(
            lang.Expr(
              lang.Expr.TupleLiteral(elems.iterator.map(_.mkNode)).like(lit),
            ),
          )
        | on(
          parent(lang.Expr) *>
            tok(TLAReader.SqBracketsGroup) *>
            children:
              field(
                repeatedSepBy1(`,`)(
                  field(TLAReader.Alpha)
                    ~ skip(`|->`)
                    ~ field(rawExpression)
                    ~ trailing,
                ),
              )
                ~ eof,
        ).rewrite: fields =>
          splice(
            lang.Expr(
              lang.Expr.RecordLiteral(
                fields.iterator.map((alpha, expr) =>
                  lang.Expr.RecordLiteral.Field(
                    lang.Id().like(alpha.unparent()),
                    expr.mkNode,
                  ),
                ),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            field(lang.Expr)
            ~ skip(defns.`.`)
            ~ field(
              lang.Expr.withChildren(
                field(lang.Id)
                  ~ eof,
              ),
            )
            ~ trailing,
        ).rewrite: (expr, id) =>
          splice(
            lang.Expr(
              lang.Expr.Project(
                lang.Expr(
                  expr.unparent(),
                ),
                id.unparent(),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            tok(TLAReader.SqBracketsGroup) *>
            children:
              field(
                repeatedSepBy1(`,`)(
                  field(TLAReader.Alpha)
                    ~ skip(`:`)
                    ~ field(rawExpression)
                    ~ trailing,
                ),
              )
                ~ eof,
        ).rewrite: fields =>
          splice(
            lang.Expr(
              lang.Expr.RecordSetLiteral(
                fields.iterator.map((alpha, expr) =>
                  lang.Expr.RecordSetLiteral.Field(
                    lang.Id().like(alpha.unparent()),
                    expr.mkNode,
                  ),
                ),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            field(lang.Expr)
            ~ field(tok(defns.InfixOperator.instances.filter(_ != defns.`.`)*))
            ~ field(lang.Expr)
            ~ eof,
        ).rewrite: (left, op, right) =>
          splice(
            lang.Expr(
              TmpInfixGroup(
                op.unparent(),
                left.unparent(),
                right.unparent(),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            field(tok(defns.PrefixOperator.instances*))
            ~ field(lang.Expr)
            ~ eof,
        ).rewrite: (op, expr) =>
          splice(
            lang.Expr(
              TmpUnaryGroup(
                op.unparent(),
                expr.unparent(),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            field(lang.Expr)
            ~ field(tok(defns.PostfixOperator.instances*))
            ~ trailing,
        ).rewrite: (expr, op) =>
          splice(
            lang.Expr(
              TmpUnaryGroup(
                op.unparent(),
                expr.unparent(),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            field(lang.Expr)
            ~ field(
              tok(TLAReader.SqBracketsGroup) *>
                children:
                  field(repeatedSepBy(`,`)(rawExpression))
                    ~ eof,
            )
            ~ eof,
        ).rewrite: (callee, args) =>
          splice(
            lang.Expr(
              lang.Expr.FnCall(
                callee.unparent(),
                args match
                  case List(expr) =>
                    expr.mkNode
                  case _ =>
                    lang.Expr(
                      lang.Expr.TupleLiteral(
                        args.iterator.map(_.mkNode),
                      ),
                    ),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            skip(defns.CASE)
            ~ field(
              repeatedSepBy(defns.`[]`)(
                field(lang.Expr)
                  ~ skip(defns.-)
                  ~ skip(defns.>)
                  ~ field(lang.Expr)
                  ~ trailing,
              ),
            )
            ~ field(
              optional(
                skip(defns.OTHER)
                  ~ skip(defns.-)
                  ~ skip(defns.>)
                  ~ field(lang.Expr)
                  ~ eof,
              ),
            )
            ~ eof,
        ).rewrite: (cases, other) =>
          splice(
            lang.Expr(
              lang.Expr.Case(
                lang.Expr.Case.Branches(
                  cases.iterator.map((pred, branch) =>
                    lang.Expr.Case.Branch(
                      pred.unparent(),
                      branch.unparent(),
                    ),
                  ),
                ),
                lang.Expr.Case.Other(
                  other match
                    case None       => lang.Expr.Case.Other.None()
                    case Some(expr) => expr.unparent(),
                ),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            field(
              tok(TLAReader.LetGroup).product(
                children(
                  field(
                    repeated1:
                      tok(lang.Operator)
                        | tok(lang.ModuleDefinition)
                        | tok(lang.Recursive),
                  )
                    ~ eof,
                ),
              ),
            )
            ~ field(lang.Expr)
            ~ trailing,
        ).rewrite: (let, expr) =>
          splice(
            lang
              .Expr(
                lang.Expr.Let(
                  lang.Expr.Let.Defns(
                    let._2.iterator.map(_.unparent()),
                  ),
                  expr.unparent(),
                ),
              )
              .like(let._1),
          )
        // TODO: Function
        // TODO: SetComprehension {expr : x \in y}
        // TODO: SetRefinement   {x \in y : bool}
        // {x \in y : p \in q} TODO: look in the book
        // TODO: Choose
        // TODO: Except
        // TODO: Lambda
        // TODO a \x b \ a
        | on(
          parent(lang.Expr) *>
            skip(defns.IF)
            ~ field(lang.Expr)
            ~ skip(defns.THEN)
            ~ field(lang.Expr)
            ~ skip(defns.ELSE)
            ~ field(lang.Expr)
            ~ trailing,
        ).rewrite: (pred, t, f) =>
          splice(
            lang.Expr.If(
              pred.unparent(),
              t.unparent(),
              f.unparent(),
            ),
          )
        | on(
          parent(lang.Expr) *>
            tok(TLAReader.ParenthesesGroup) *>
            children:
              field(rawExpression)
                ~ eof,
        ).rewrite: rawExpr =>
          splice(
            lang.Expr(rawExpr.mkNode),
          )
    *> pass(once = false, strategy = pass.bottomUp) // resolve Alphas
      .rules:
        on(
          parent(lang.Expr) *>
            lang.Id,
        ).rewrite: name =>
          splice(
            lang.Expr(
              lang.Expr.OpCall(
                name.unparent(),
                lang.Expr.OpCall.Params(),
              ),
            ),
          )
  end buildExpressions

  val reorderOperations = passDef:
    wellformed := prevWellformed.makeDerived:
      lang.Expr.removeCases(TmpInfixGroup, TmpUnaryGroup)
    pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ highPredInfixInfix(op)
                    ~ field(lang.Expr)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr1, expr2) =>
              splice(
                TmpInfixGroup(
                  op.unparent(),
                  expr1.unparent(),
                  lang.Expr(
                    TmpInfixGroup(
                      curOp.unparent(),
                      expr2.unparent(),
                      right.unparent(),
                    ),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ field(lang.Expr)
                    ~ highPredInfixInfix(op)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr1, expr2) =>
              splice(
                TmpInfixGroup(
                  op.unparent(),
                  lang.Expr(
                    TmpInfixGroup(
                      curOp.unparent(),
                      left.unparent(),
                      expr1.unparent(),
                    ),
                  ),
                  expr2.unparent(),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ highPredInfixUnary(op)
                    ~ field(lang.Expr)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr) =>
              splice(
                TmpUnaryGroup(
                  op.unparent(),
                  lang.Expr(
                    TmpInfixGroup(
                      curOp.unparent(),
                      expr.unparent(),
                      right.unparent(),
                    ),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ field(lang.Expr)
                    ~ highPredInfixUnary(op)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr) =>
              splice(
                TmpUnaryGroup(
                  op.unparent(),
                  lang.Expr(
                    TmpInfixGroup(
                      curOp.unparent(),
                      left.unparent(),
                      expr.unparent(),
                    ),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpUnaryGroup.withChildren:
              defns.PrefixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ highPredUnaryInfix(op)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                TmpInfixGroup(
                  op.unparent(),
                  lang.Expr(
                    TmpUnaryGroup(
                      curOp.unparent(),
                      left.unparent(),
                    ),
                  ),
                  right.unparent(),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpUnaryGroup.withChildren:
              defns.PostfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ highPredUnaryInfix(op)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                TmpInfixGroup(
                  op.unparent(),
                  left.unparent(),
                  lang.Expr(
                    TmpUnaryGroup(
                      curOp.unparent(),
                      right.unparent(),
                    ),
                  ),
                ),
              )
    *> pass(once = false, strategy = pass.bottomUp) // assoc related errors
      .rules:
        on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ badPredInfixInfix(op)
                    ~ field(lang.Expr)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr1, expr2) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message( // todo: use src
                    s"$curOp and $op must have different precedence, or be duplicates of an associative operator.",
                  ),
                  Builtin.Error.AST(
                    TmpInfixGroup(
                      curOp.unparent(),
                      TmpInfixGroup(
                        op.unparent(),
                        expr1.unparent(),
                        expr2.unparent(),
                      ),
                      right.unparent(),
                    ),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ field(lang.Expr)
                    ~ badPredInfixInfix((op))
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr1, expr2) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence, or be duplicates of an associative operator.",
                  ),
                  Builtin.Error.AST(
                    TmpInfixGroup(
                      curOp.unparent(),
                      left.unparent(),
                      TmpInfixGroup(
                        op.unparent(),
                        expr1.unparent(),
                        expr2.unparent(),
                      ),
                    ),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ badPredInfixUnary(op)
                    ~ field(lang.Expr)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          left match
            case (op, expr) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence.",
                  ),
                  Builtin.Error.AST(
                    TmpInfixGroup(
                      curOp.unparent(),
                      TmpUnaryGroup(
                        op.unparent(),
                        expr.unparent(),
                      ),
                    ),
                    right.unparent(),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpInfixGroup.withChildren:
              defns.InfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ field(lang.Expr)
                    ~ badPredInfixUnary(op)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, left, right) =>
          right match
            case (op, expr) =>
              splice(
                Node(Builtin.Error)(
                  Builtin.Error.Message(
                    s"$curOp and $op must have different precedence.",
                  ),
                  Builtin.Error.AST(
                    TmpInfixGroup(
                      curOp.unparent(),
                      left.unparent(),
                      TmpUnaryGroup(
                        op.unparent(),
                        expr.unparent(),
                      ),
                    ),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpUnaryGroup.withChildren:
              defns.PrefixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ badPredUnaryInfix(op)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                Builtin.Error(
                  s"$curOp and $op must have different precedence.",
                  curOp.unparent(),
                  TmpInfixGroup(
                    op.unparent(),
                    left.unparent(),
                    right.unparent(),
                  ),
                ),
              )
        | on(
          parent(lang.Expr) *>
            field(TmpUnaryGroup.withChildren:
              defns.PostfixOperator.instances.iterator
                .map: op =>
                  field(op)
                    ~ badPredUnaryInfix(op)
                    ~ eof
                .reduce(_ | _))
            ~ eof,
        ).rewrite: (curOp, infixGroup) =>
          infixGroup match
            case (op, left, right) =>
              splice(
                Builtin.Error(
                  s"$curOp and $op must have different precedence.",
                  curOp.unparent(),
                  TmpInfixGroup(
                    op.unparent(),
                    left.unparent(),
                    right.unparent(),
                  ),
                ),
              )
    *> pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          parent(lang.Expr) *>
            field(
              TmpInfixGroup.withChildren(
                field(tok(defns.InfixOperator.instances*))
                  ~ field(lang.Expr)
                  ~ field(lang.Expr)
                  ~ eof,
              ),
            )
            ~ eof,
        ).rewrite: (op, right, left) =>
          splice(
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(op.unparent()),
                lang.Expr.OpCall.Params(
                  right.unparent(),
                  left.unparent(),
                ),
              ),
            ),
          )
        | on(
          parent(lang.Expr) *>
            field(
              TmpUnaryGroup.withChildren(
                field(
                  tok(defns.PrefixOperator.instances*)
                    | tok(defns.PostfixOperator.instances*),
                )
                  ~ field(lang.Expr)
                  ~ eof,
              ),
            )
            ~ eof,
        ).rewrite: (op, expr) =>
          splice(
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(op.unparent()),
                lang.Expr.OpCall.Params(
                  expr.unparent(),
                ),
              ),
            ),
          )
  end reorderOperations

  val removeNestedExpr = passDef:
    wellformed := prevWellformed.makeDerived:
      lang.Expr.removeCases(lang.Expr)

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
