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

package forja.langs.calc

import cats.syntax.all.given

import forja.*
import forja.dsl.*
import forja.source.Reader
import forja.wf.Wellformed

import lang.*

object CalcEvaluator extends PassSeq:
  import Reader.*
  import CalcReader.*

  def inputWellformed: Wellformed = lang.wf

  val passes = List(
    ConstantsPass,
    EvaluatorPass,
    StripExpressionPass,
  )

  object ConstantsPass extends Pass:
    val wellformed = prevWellformed.makeDerived:
      Expression.removeCases(Number)
      Expression.addCases(EmbedMeta[Int])
    val rules = pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(
          tok(Expression) *> onlyChild(tok(Number)),
        ).rewrite: num =>
          splice(Expression(Node.Embed(num.sourceRange.decodeString().toInt)))

  object EvaluatorPass extends Pass:
    val wellformed = prevWellformed.makeDerived:
      Expression ::=! embedded[Int]
    val rules = pass(once = false, strategy = pass.bottomUp)
      .rules:
        on(
          tok(Expression) *> onlyChild(
            tok(Add).withChildren:
              field(tok(Expression) *> onlyChild(embed[Int]))
                ~ field(tok(Expression) *> onlyChild(embed[Int]))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          splice(Expression(Node.Embed[Int](left + right)))
        | on(
          tok(Expression) *> onlyChild(
            tok(Sub).withChildren:
              field(tok(Expression) *> onlyChild(embed[Int]))
                ~ field(tok(Expression) *> onlyChild(embed[Int]))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          splice(Expression(Node.Embed[Int](left - right)))
        | on(
          tok(Expression) *> onlyChild(
            tok(Mul).withChildren:
              field(tok(Expression) *> onlyChild(embed[Int]))
                ~ field(tok(Expression) *> onlyChild(embed[Int]))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          splice(Expression(Node.Embed[Int](left * right)))
        | on(
          tok(Expression) *> onlyChild(
            tok(Div).withChildren:
              field(tok(Expression) *> onlyChild(embed[Int]))
                ~ field(tok(Expression) *> onlyChild(embed[Int]))
                ~ eof,
          ),
        ).rewrite: (left, right) =>
          splice(Expression(Node.Embed[Int](left / right)))
    end rules
  end EvaluatorPass

  object StripExpressionPass extends Pass:
    val wellformed = prevWellformed.makeDerived:
      Node.Top ::=! embedded[Int]
    val rules = pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(tok(Expression) *> onlyChild(embed[Int])).rewrite: i =>
          splice(Node.Embed(i))
  end StripExpressionPass
end CalcEvaluator
