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
        // instrumentWithTracer(forja.manip.RewriteDebugTracer(os.pwd / "dbg_exprmarker")):
        ExprMarker(freshTop)
      Node.Top(
        freshTop(lang.Module)(lang.Module.Defns)(lang.Operator)(
          lang.Expr,
        ).unparentedChildren,
      )

  test("NumberLiteral"):
    assertEquals(
        Node.Top(ExprTry(), TLAReader.NumberLiteral("1")).parseNode,
        Node.Top(lang.Expr(lang.Expr.NumberLiteral("1"))),
    )

  test("StringLiteral"):
    assertEquals(
        Node.Top(ExprTry(), TLAReader.StringLiteral("string")).parseNode,
        Node.Top(lang.Expr(lang.Expr.StringLiteral("string"))),
    )

  test("ParenthesesGroup"):
    assertEquals(
        Node.Top(ExprTry(), TLAReader.ParenthesesGroup(
          TLAReader.NumberLiteral("1")
        )).parseNode,
        Node.Top(lang.Expr(lang.Expr.NumberLiteral("1"))),
    )
    // TODO: paranthesis with op


  test("Set Literal"):
    // empty set    
    assertEquals(
      Node.Top(ExprTry(), TLAReader.BracesGroup()).parseNode,
      Node.Top(lang.Expr(lang.Expr.SetLiteral()))
    )
    // set with three elements
    assertEquals(
      Node.Top(ExprTry(), TLAReader.BracesGroup(
        TLAReader.NumberLiteral("1"),
        TLAReader.`,`(","),
        TLAReader.NumberLiteral("2"),
        TLAReader.`,`(","),
        TLAReader.NumberLiteral("3")
      )).parseNode,
      Node.Top(
        lang.Expr(
          lang.Expr.SetLiteral(
            lang.Expr(lang.Expr.NumberLiteral("1")),
            lang.Expr(lang.Expr.NumberLiteral("2")),
            lang.Expr(lang.Expr.NumberLiteral("3")),
          ),
        )
      )
    )
    // nested sets
    assertEquals(
      Node.Top(ExprTry(), TLAReader.BracesGroup(
        TLAReader.NumberLiteral("1"),
        TLAReader.`,`(","),
        TLAReader.BracesGroup(),
        TLAReader.`,`(","),
        TLAReader.NumberLiteral("3")
      )).parseNode,
      Node.Top(
        lang.Expr(
          lang.Expr.SetLiteral(
            lang.Expr(lang.Expr.NumberLiteral("1")),
            lang.Expr(lang.Expr.SetLiteral()),
            lang.Expr(lang.Expr.NumberLiteral("3")),
          ),
        )
      )
    )

  test("Tuple Literal"):
    // empty tuple
    assertEquals(
      Node.Top(ExprTry(), TLAReader.TupleGroup()).parseNode,
      Node.Top(lang.Expr(lang.Expr.TupleLiteral()))
    )
    // tuple with three elements
    assertEquals(
      Node.Top(ExprTry(), TLAReader.TupleGroup(
        TLAReader.NumberLiteral("1"),
        TLAReader.`,`(","),
        TLAReader.StringLiteral("two"),
        TLAReader.`,`(","),
        TLAReader.NumberLiteral("3")
      )).parseNode,
      Node.Top(
        lang.Expr(
          lang.Expr.TupleLiteral(
            lang.Expr(lang.Expr.NumberLiteral("1")),
            lang.Expr(lang.Expr.StringLiteral("two")),
            lang.Expr(lang.Expr.NumberLiteral("3")),
          ),
        )
      )
    )
  


  test("Case"):
    assertEquals(
      Node.Top(
        ExprTry(), 
        defns.CASE(), TLAReader.StringLiteral("A"), TLAReader.`->`("->"), TLAReader.NumberLiteral("1")
      ).parseNode,
      Node.Top(
          lang.Expr(
          lang.Expr.Case(
            lang.Expr.Case.Branches(
              lang.Expr.Case.Branch(
                lang.Expr(lang.Expr.StringLiteral("A")),
                lang.Expr(lang.Expr.NumberLiteral("1")),
              ),
            ),
            lang.Expr.Case.Other(lang.Expr.Case.None()),
          ),
        )
      )
    )
    assertEquals(
      Node.Top(
        ExprTry(), 
        defns.CASE(), TLAReader.StringLiteral("A"), TLAReader.`->`("->"), TLAReader.NumberLiteral("1"),
        defns.`[]`("[]"), TLAReader.StringLiteral("B"), TLAReader.`->`("->"), TLAReader.NumberLiteral("2"),

      ).parseNode,
      Node.Top(
          lang.Expr(
          lang.Expr.Case(
            lang.Expr.Case.Branches(
              lang.Expr.Case.Branch(
                lang.Expr(lang.Expr.StringLiteral("A")),
                lang.Expr(lang.Expr.NumberLiteral("1")),
              ),
              lang.Expr.Case.Branch(
                lang.Expr(lang.Expr.StringLiteral("B")),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
            lang.Expr.Case.Other(lang.Expr.Case.None()),
          ),
        )
      )
    )
    assertEquals(
      Node.Top(
        ExprTry(), 
        defns.CASE(), TLAReader.StringLiteral("A"), TLAReader.`->`("->"), TLAReader.NumberLiteral("1"),
        defns.`[]`("[]"), TLAReader.StringLiteral("B"), TLAReader.`->`("->"), TLAReader.NumberLiteral("2"),
        defns.OTHER("OTHER"), TLAReader.`->`("->"), TLAReader.NumberLiteral("3"),

      ).parseNode,
      Node.Top(
          lang.Expr(
          lang.Expr.Case(
            lang.Expr.Case.Branches(
              lang.Expr.Case.Branch(
                lang.Expr(lang.Expr.StringLiteral("A")),
                lang.Expr(lang.Expr.NumberLiteral("1")),
              ),
              lang.Expr.Case.Branch(
                lang.Expr(lang.Expr.StringLiteral("B")),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
            lang.Expr.Case.Other(lang.Expr(lang.Expr.NumberLiteral("3"))),
          ),
        )
      )
    )


  // test("LET"):
  //   assertEquals(
  //     """LET x == 1
  //       |y == 2
  //       |IN  {y, x}""".stripMargin.parseStr,
  //     Node.Top(
  //       lang.Expr.Let(
  //         lang.Expr.Let.Defns(
  //           lang.Operator(
  //             lang.Id("x"),
  //             lang.Operator.Params(),
  //             lang.Expr(lang.Expr.NumberLiteral("1")),
  //           ),
  //           lang.Operator(
  //             lang.Id("y"),
  //             lang.Operator.Params(),
  //             lang.Expr(lang.Expr.NumberLiteral("2")),
  //           ),
  //         ),
  //         lang.Expr(
  //           lang.Expr.SetLiteral(
  //             lang.Expr(
  //               lang.Expr.OpCall(
  //                 lang.Id("y"),
  //                 lang.Expr.OpCall.Params(),
  //               ),
  //             ),
  //             lang.Expr(
  //               lang.Expr.OpCall(
  //                 lang.Id("x"),
  //                 lang.Expr.OpCall.Params(),
  //               ),
  //             ),
  //           ),
  //         ),
  //       ),
  //     ),
  //   )

