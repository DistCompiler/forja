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
import forja.source.SourceRange
import forja.wf.WellformedDef

object lang extends WellformedDef:
  lazy val topShape: Shape = repeated(Expression)

  object Expression
      extends t(
        choice(
          Number,
          Add,
          Sub,
          Mul,
          Div,
        ),
      )

  object Number extends t(Atom), Token.ShowSource

  object Add
      extends t(
        fields(
          Expression,
          Expression,
        ),
      )
  object Sub
      extends t(
        fields(
          Expression,
          Expression,
        ),
      )
  object Mul
      extends t(
        fields(
          Expression,
          Expression,
        ),
      )
  object Div
      extends t(
        fields(
          Expression,
          Expression,
        ),
      )

object read:
  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    CalcReader(sourceRange)
