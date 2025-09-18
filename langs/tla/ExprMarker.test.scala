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

import forja.*
import forja.dsl.*
// import forja.source.{Source, SourceRange}
import ExprMarker.ExprTry

// Run with:
// scala-cli test . -- '*ExprMarker*'

class ExprMarkerTests extends munit.FunSuite:
  extension (top: Node.Top)
    def parseNode: Node.Top =
      val freshTop = Node.Top(
        lang.Module(
          lang.Id("TestMod"),
          lang.Module.Extends(),
          lang.Module.Defns(
            lang.Operator(
              lang.Id("test"),
              lang.Operator.Params(),
              lang.Expr(
                top.unparentedChildren,
              ),
            ),
          ),
        ),
      )
      ExprMarker(freshTop)
      Node.Top(
        freshTop(lang.Module)(lang.Module.Defns)(lang.Operator)(
          lang.Expr,
        ).unparentedChildren,
      )

  test("NumberLiteral"):
    assertEquals(
        Node.Top(TLAReader.NumberLiteral("1")).parseNode,
        Node.Top(lang.Expr(lang.Expr.NumberLiteral("1"))),
    )
    assertEquals(
        Node.Top(ExprTry(), TLAReader.NumberLiteral("1")).parseNode,
        Node.Top(lang.Expr(lang.Expr.NumberLiteral("1"))),
    )

  test("StringLiteral"):
    assertEquals(
        Node.Top(TLAReader.StringLiteral("string")).parseNode,
        Node.Top(lang.Expr(lang.Expr.StringLiteral("string"))),
    )

    assertEquals(
        Node.Top(ExprTry(), TLAReader.StringLiteral("string")).parseNode,
        Node.Top(lang.Expr(lang.Expr.StringLiteral("string"))),
    )

  test("Alpha"):
      assertEquals(
        Node.Top(ExprTry(), TLAReader.Alpha("x")).parseNode,
        Node.Top(
          lang.Expr(lang.Expr.OpCall(lang.Id("x"), lang.Expr.OpCall.Params()))
        )
      )

  // test("Set Literal"):
  //   assertEquals(
  //     Node.Top(ExprTry(), TLAReader.BracesGroup(
  //       TLAReader.NumberLiteral("1"),
  //       TLAReader.`,`(","),
  //       TLAReader.NumberLiteral("2"),
  //       TLAReader.`,`(","),
  //       TLAReader.NumberLiteral("3")
  //     )).parseNode,
  //     Node.Top(
  //       lang.Expr(
  //         lang.Expr.SetLiteral(
  //           lang.Expr(lang.Expr.NumberLiteral("1")),
  //           lang.Expr(lang.Expr.NumberLiteral("2")),
  //           lang.Expr(lang.Expr.NumberLiteral("3")),
  //         ),
  //       )
  //     )
  //   )
  //   assertEquals(
  //     Node.Top(ExprTry(), TLAReader.BracesGroup(
  //       TLAReader.NumberLiteral("1"),
  //       TLAReader.`,`(","),
  //       TLAReader.BracesGroup(),
  //       TLAReader.`,`(","),
  //       TLAReader.NumberLiteral("3")
  //     )).parseNode,
  //     Node.Top(
  //       lang.Expr(
  //         lang.Expr.SetLiteral(
  //           lang.Expr(lang.Expr.NumberLiteral("1")),
  //           lang.Expr(lang.Expr.SetLiteral()),
  //           lang.Expr(lang.Expr.NumberLiteral("3")),
  //         ),
  //       )
  //     )
  //   )
  //   assertEquals(
  //     Node.Top(ExprTry(), TLAReader.BracesGroup()).parseNode,
  //     Node.Top(lang.Expr(lang.Expr.SetLiteral())),
  //   )