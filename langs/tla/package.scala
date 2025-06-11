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
import forja.wf.WellformedDef

object lang extends WellformedDef:
  lazy val topShape: Shape = repeated(Module)

  object Module
      extends t(
        fields(
          Id,
          Module.Extends,
          Module.Defns,
        ),
      ):
    object Extends extends t(repeated(Id))
    object Defns
        extends t(
          repeated(
            choice(
              Local,
              Recursive,
              Operator,
              Variable,
              Constant,
              Assumption,
              Theorem,
              Instance,
              Module,
              ModuleDefinition,
            ),
          ),
        )
  end Module

  object Local
      extends t(
        choice(
          Operator,
          Instance,
          ModuleDefinition,
        ),
      )

  object Recursive
      extends t(
        choice(
          Id,
          Order2,
        ),
      )

  object ModuleDefinition
      extends t(
        fields(
          Id,
          Operator.Params,
          Instance,
        ),
      )

  object Id extends t(Atom)

  object Ids extends t(repeated(Id, minCount = 1))

  object OpSym extends t(choice(defns.Operator.instances.toSet))
  defns.Operator.instances.foreach(_ ::= Atom)

  object Order2
      extends t(
        fields(
          Id,
          embedded[Int],
        ),
      )

  object Expr
      extends t(
        choice(
          Expr.NumberLiteral,
          Expr.StringLiteral,
          Expr.SetLiteral,
          Expr.TupleLiteral,
          Expr.RecordLiteral,
          Expr.Project,
          Expr.RecordSetLiteral,
          Expr.OpCall,
          Expr.FnCall,
          Expr.If,
          Expr.Case,
          Expr.Let,
          Expr.Exists,
          Expr.Forall,
          Expr.Function,
          Expr.SetComprehension,
          Expr.SetRefinement,
          Expr.Choose,
          Expr.Except,
          Expr.Except.Anchor,
          Expr.Lambda,
        ),
      ):
    object NumberLiteral extends t(Atom)

    object StringLiteral extends t(Atom)

    object SetLiteral extends t(repeated(Expr))

    object TupleLiteral extends t(repeated(Expr))

    object RecordLiteral extends t(repeated(RecordLiteral.Field, minCount = 1)):
      object Field
          extends t(
            fields(
              Id,
              Expr,
            ),
          )
    end RecordLiteral

    object Project
        extends t(
          fields(
            Expr,
            Id,
          ),
        )

    object RecordSetLiteral
        extends t(
          repeated(
            RecordSetLiteral.Field,
            minCount = 1,
          ),
        ):
      object Field
          extends t(
            fields(
              Id,
              Expr,
            ),
          )
    end RecordSetLiteral

    object OpCall
        extends t(
          fields(
            choice(Id, OpSym),
            OpCall.Params,
          ),
        ):
      object Params extends t(repeated(Expr))
    end OpCall

    object FnCall extends t(fields(Expr, Expr))

    object If
        extends t(
          fields(
            Expr,
            Expr,
            Expr,
          ),
        )

    object Case extends t(fields(Case.Branches, Case.Other)):
      object Branches extends t(repeated(Branch, minCount = 1))
      object Branch
          extends t(
            fields(
              Expr,
              Expr,
            ),
          )
      object Other extends t(choice(Expr, None))
      object None extends t(Atom)
    end Case

    object Let
        extends t(
          fields(
            Let.Defns,
            Expr,
          ),
        ):
      object Defns
          extends t(
            repeated(
              choice(
                Operator,
                ModuleDefinition,
                Recursive,
              ),
              minCount = 1,
            ),
          )
    end Let

    object Exists
        extends t(
          fields(
            QuantifierBounds,
            Expr,
          ),
        )

    object Forall
        extends t(
          fields(
            QuantifierBounds,
            Expr,
          ),
        )

    object Function
        extends t(
          fields(
            QuantifierBounds,
            Expr,
          ),
        )

    object SetComprehension
        extends t(
          fields(
            Expr,
            QuantifierBounds,
          ),
        )

    object SetRefinement
        extends t(
          fields(
            QuantifierBound,
            Expr,
          ),
        )

    object Choose
        extends t(
          fields(
            QuantifierBound,
            Expr,
          ),
        )

    object Except
        extends t(
          fields(
            Expr,
            Except.Substitutions,
          ),
        ):
      object Substitutions
          extends t(
            repeated(
              Substitution,
              minCount = 1,
            ),
          )

      object Substitution
          extends t(
            fields(
              Path,
              Expr,
            ),
          )

      object Path extends t(repeated(Expr, minCount = 1))

      object Anchor extends t(Atom)
    end Except

    object Lambda
        extends t(
          fields(
            Lambda.Params,
            Expr,
          ),
        ):
      object Params extends t(repeated(Id, minCount = 1))
    end Lambda
  end Expr

  object Operator
      extends t(
        fields(
          choice(Id, OpSym),
          Operator.Params,
          Expr,
        ),
      ):
    object Params
        extends t(
          repeated(
            choice(
              Id,
              Order2,
            ),
          ),
        )
  end Operator

  object Variable extends t(Id)

  object Constant extends t(choice(Id, Order2))

  object Anonymous extends t(Atom)

  object Assumption
      extends t(
        fields(
          choice(Id, Anonymous),
          Expr,
        ),
      )

  object Theorem
      extends t(
        fields(
          choice(Id, Anonymous),
          choice(Expr, Theorem.AssumeProve),
          Theorem.Proofs,
        ),
      ):
    object AssumeProve extends t(AnyShape)
    object Proofs extends t(AnyShape)
  end Theorem

  object UseOrHide extends t(AnyShape)
  forceDef(UseOrHide)

  object Instance
      extends t(
        fields(
          Id,
          Instance.Substitutions,
        ),
      ):
    object Substitutions extends t(repeated(Substitution))
    object Substitution
        extends t(
          fields(
            choice(Id, OpSym),
            Expr,
          ),
        )
  end Instance

  object QuantifierBound
      extends t(
        fields(
          choice(Id, Ids),
          Expr,
        ),
      )

  object QuantifierBounds extends t(repeated(QuantifierBound, minCount = 1))
end lang
