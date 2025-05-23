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
import forja.source.{Source, SourceRange}

import ExprParser.TmpInfixGroup

// scala-cli test . -- '*ExprParser*'

class ExprParserTests extends munit.FunSuite:
  extension (str: String)
    def parseStr: Node.Top =
      val wrapped_str =
        Source.fromString(
          s"""---- MODULE TestMod ----
             |EXTENDS Naturals
             |VARIABLE temp
             |
             |Init == $str
             |====
          """.stripMargin,
        )
      val top = TLAReader(SourceRange.entire(wrapped_str))
      TLAParser(top)
      ExprParser(top)
      Node.Top(
        top(lang.Module)(lang.Module.Defns)(lang.Operator)(
          lang.Expr,
        ).unparentedChildren,
      )
  extension (str: String)
    def withoutParse: Node.Top =
      val wrapped_str =
        Source.fromString(
          s"""---- MODULE TestMod ----
             |EXTENDS Naturals
             |VARIABLE temp
             |
             |Init == $str
             |====
          """.stripMargin,
        )
      val top = TLAReader(SourceRange.entire(wrapped_str))
      TLAParser(top)
      Node.Top(
        top(lang.Module)(lang.Module.Defns)(lang.Operator)(
          lang.Expr,
        ).unparentedChildren,
      )

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
      ExprParser(freshTop)
      Node.Top(
        freshTop(lang.Module)(lang.Module.Defns)(lang.Operator)(
          lang.Expr,
        ).unparentedChildren,
      )

  test("NumberLiteral"):
    assertEquals("1".parseStr, Node.Top(lang.Expr.NumberLiteral("1")))

  test("StringLiteral"):
    assertEquals(
      "\"string\"".parseStr,
      Node.Top(lang.Expr.StringLiteral("string")),
    )
    assertEquals(
      "\"string\\nnewline\"".parseStr,
      Node.Top(lang.Expr.StringLiteral("string\nnewline")),
    )

  test("Set Literal"):
    assertEquals(
      "{1, 2, 3}".parseStr,
      Node.Top(
        lang.Expr.SetLiteral(
          lang.Expr(lang.Expr.NumberLiteral("1")),
          lang.Expr(lang.Expr.NumberLiteral("2")),
          lang.Expr(lang.Expr.NumberLiteral("3")),
        ),
      ),
    )
    assertEquals(
      "{}".parseStr,
      Node.Top(lang.Expr.SetLiteral()),
    )

  test("TupleLiteral"):
    assertEquals(
      "<<1, 2, 3>>".parseStr,
      Node.Top(
        lang.Expr.TupleLiteral(
          lang.Expr(lang.Expr.NumberLiteral("1")),
          lang.Expr(lang.Expr.NumberLiteral("2")),
          lang.Expr(lang.Expr.NumberLiteral("3")),
        ),
      ),
    )
    assertEquals(
      "<<>>".parseStr,
      Node.Top(
        lang.Expr.TupleLiteral(),
      ),
    )

  test("RecordLiteral"):
    assertEquals(
      "[x |-> 2, y |-> 3]".parseStr,
      Node.Top(
        lang.Expr.RecordLiteral(
          lang.Expr.RecordLiteral.Field(
            lang.Id("x"),
            lang.Expr(lang.Expr.NumberLiteral("2")),
          ),
          lang.Expr.RecordLiteral.Field(
            lang.Id("y"),
            lang.Expr(lang.Expr.NumberLiteral("3")),
          ),
        ),
      ),
    )

  test("Projection (Record Field Acess)"):
    assertEquals(
      "x.y".parseStr,
      Node.Top(
        lang.Expr.Project(
          lang.Expr(
            lang.Expr.OpCall(
              lang.Id("x"),
              lang.Expr.OpCall.Params(),
            ),
          ),
          lang.Id("y"),
        ),
      ),
    )
    assertEquals(
      "x.y.z".parseStr,
      Node.Top(
        lang.Expr.Project(
          lang.Expr(
            lang.Expr.Project(
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id("x"),
                  lang.Expr.OpCall.Params(),
                ),
              ),
              lang.Id("y"),
            ),
          ),
          lang.Id("z"),
        ),
      ),
    )
    assertEquals(
      "[y |-> 2].y".parseStr,
      Node.Top(
        lang.Expr.Project(
          lang.Expr(
            lang.Expr.RecordLiteral(
              lang.Expr.RecordLiteral.Field(
                lang.Id("y"),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
          ),
          lang.Id("y"),
        ),
      ),
    )

  test("RecordSetLiteral"):
    assertEquals(
      "[x : {1, 2}]".parseStr,
      Node.Top(
        lang.Expr.RecordSetLiteral(
          lang.Expr.RecordSetLiteral.Field(
            lang.Id("x"),
            lang.Expr(
              lang.Expr.SetLiteral(
                lang.Expr(lang.Expr.NumberLiteral("1")),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
          ),
        ),
      ),
    )

  test("Name"):
    assertEquals(
      "x".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.Id("x"),
          lang.Expr.OpCall.Params(),
        ),
      ),
    )

  test("Single Binary Operator"):
    assertEquals(
      "5 + 6".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(
            defns.+("+"),
          ),
          lang.Expr.OpCall.Params(
            lang.Expr(lang.Expr.NumberLiteral("5")),
            lang.Expr(lang.Expr.NumberLiteral("6")),
          ),
        ),
      ),
    )
    assertEquals(
      "5 $ x".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.$("$")),
          lang.Expr.OpCall.Params(
            lang.Expr(lang.Expr.NumberLiteral("5")),
            lang.Expr(
              lang.Expr.OpCall(
                lang.Id("x"),
                lang.Expr.OpCall.Params(),
              ),
            ),
          ),
        ),
      ),
    )

  test("Single Unary Operator"):
    assertEquals(
      "UNION A".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.UNION("UNION")),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.Id("A"),
                lang.Expr.OpCall.Params(),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(
      "x'".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.`'`("'")),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.Id("x"),
                lang.Expr.OpCall.Params(),
              ),
            ),
          ),
        ),
      ),
    )

  test("Precedence: Infix - Infix"):
    assertEquals(
      "5 * 6 + 7".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(
            defns.+("+"),
          ),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(
                  defns.*("*"),
                ),
                lang.Expr.OpCall.Params(
                  lang.Expr(lang.Expr.NumberLiteral("5")),
                  lang.Expr(lang.Expr.NumberLiteral("6")),
                ),
              ),
            ),
            lang.Expr(lang.Expr.NumberLiteral("7")),
          ),
        ),
      ),
    )
    assertEquals(
      "1 + 8 * 6 - 9 * 3".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.+("+")),
          lang.Expr.OpCall.Params(
            lang.Expr(lang.Expr.NumberLiteral("1")),
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(defns.-("-")),
                lang.Expr.OpCall.Params(
                  lang.Expr(
                    lang.Expr.OpCall(
                      lang.OpSym(defns.*("*")),
                      lang.Expr.OpCall.Params(
                        lang.Expr(lang.Expr.NumberLiteral("8")),
                        lang.Expr(lang.Expr.NumberLiteral("6")),
                      ),
                    ),
                  ),
                  lang.Expr(
                    lang.Expr.OpCall(
                      lang.OpSym(defns.*("*")),
                      lang.Expr.OpCall.Params(
                        lang.Expr(lang.Expr.NumberLiteral("9")),
                        lang.Expr(lang.Expr.NumberLiteral("3")),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )

  test("Precedence: Infix - Prefix"):
    assertEquals(
      "~x /\\ y".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns./\("/\\")),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(defns.~("~")),
                lang.Expr.OpCall.Params(
                  lang.Expr(
                    lang.Expr.OpCall(
                      lang.Id("x"),
                      lang.Expr.OpCall.Params(),
                    ),
                  ),
                ),
              ),
            ),
            lang.Expr(
              lang.Expr.OpCall(
                lang.Id("y"),
                lang.Expr.OpCall.Params(),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(
      "~x \\in y".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.~("~")),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(defns.`\\in`("\\in")),
                lang.Expr.OpCall.Params(
                  lang.Expr(
                    lang.Expr.OpCall(
                      lang.Id("x"),
                      lang.Expr.OpCall.Params(),
                    ),
                  ),
                  lang.Expr(
                    lang.Expr.OpCall(
                      lang.Id("y"),
                      lang.Expr.OpCall.Params(),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )

  test("Precedence: Infix - Postfix"):
    assertEquals(
      "1 + x' * 2".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.+("+")),
          lang.Expr.OpCall.Params(
            lang.Expr(lang.Expr.NumberLiteral("1")),
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(defns.*("*")),
                lang.Expr.OpCall.Params(
                  lang.Expr(
                    lang.Expr.OpCall(
                      lang.OpSym(defns.`'`("'")),
                      lang.Expr.OpCall.Params(
                        lang.Expr(
                          lang.Expr.OpCall(
                            lang.Id("x"),
                            lang.Expr.OpCall.Params(),
                          ),
                        ),
                      ),
                    ),
                  ),
                  lang.Expr(lang.Expr.NumberLiteral("2")),
                ),
              ),
            ),
          ),
        ),
      ),
    )

  test("Precedence: Unary - Unary"):
    assertEquals(
      "[] ~x".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.`[]`("[]")),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(defns.~("~")),
                lang.Expr.OpCall.Params(
                  lang.Expr(
                    lang.Expr.OpCall(
                      lang.Id("x"),
                      lang.Expr.OpCall.Params(),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )

    // TODO???: postfix postfix: never allowed

  test("Precedence: Associative"):
    assertEquals(
      "1 + 2 + 3".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns.+("+")),
          lang.Expr.OpCall.Params(
            lang.Expr(lang.Expr.NumberLiteral("1")),
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(defns.+("+")),
                lang.Expr.OpCall.Params(
                  lang.Expr(lang.Expr.NumberLiteral("2")),
                  lang.Expr(lang.Expr.NumberLiteral("3")),
                ),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(
      "1 * 2 / 3".parseStr,
      Node.Top(
        lang.Expr(
          Builtin.Error(
            s"forja.langs.tla.defns.* and forja.langs.tla.defns./ must have different precedence, or be duplicates of an associative operator.",
            TmpInfixGroup(
              defns.*("*"),
              lang.Expr(lang.Expr.NumberLiteral("1")),
              TmpInfixGroup(
                defns./("/"),
                lang.Expr(lang.Expr.NumberLiteral("2")),
                lang.Expr(lang.Expr.NumberLiteral("3")),
              ),
            ),
          ),
        ),
      ),
    )

  // test("Precedence: Error"):
  //   assertEquals(
  //     "x \\in A \\in B".parseStr,
  //     Node.Top(lang.Expr(
  //       Node(Builtin.Error)(
  //         Builtin.Error.Message(
  /* s"26:distcompiler.tla.defns.\\in and 26:distcompiler.tla.defns.\\in must
   * have different precedence, or be duplicates of an associative operator." */
  //         ),
  //         Builtin.Error.AST(
  //           lang.Expr.TmpInfixGroup(
  //             defns.`\\in`("\\in"),
  //             lang.Expr(lang.Id("x")),
  //             lang.Expr.TmpInfixGroup(
  //               defns.`\\in`("\\in"),
  //               lang.Expr(lang.Id("A")),
  //               lang.Expr(lang.Id("B"))
  //             )))))))
  //   assertEquals(
  //     "x \\in A = B".parseStr,
  //     Node.Top(lang.Expr(
  //       Node(Builtin.Error)(
  //         Builtin.Error.Message(
  /* s"26:distcompiler.tla.defns.\\in and distcompiler.tla.defns.= must have
   * different precedence, or be duplicates of an associative operator." */
  //         ),
  //         Builtin.Error.AST(
  //           lang.Expr.TmpInfixGroup(
  //             defns.`\\in`("\\in"),
  //             lang.Expr(lang.Id("x")),
  //             lang.Expr.TmpInfixGroup(
  //               defns.`=`("="),
  //               lang.Expr(lang.Id("A")),
  //               lang.Expr(lang.Id("B"))
  //             )))))))

  test("OpCall"):
    assertEquals(
      "testFun(1, 2, 3)".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.Id("testFun"),
          lang.Expr.OpCall.Params(
            lang.Expr(lang.Expr.NumberLiteral("1")),
            lang.Expr(lang.Expr.NumberLiteral("2")),
            lang.Expr(lang.Expr.NumberLiteral("3")),
          ),
        ),
      ),
    )

  // TODO: conjunction alignment

  test("FnCall"):
    assertEquals(
      "x[\"y\"]".parseStr,
      Node.Top(
        lang.Expr.FnCall(
          lang.Expr(
            lang.Expr.OpCall(
              lang.Id("x"),
              lang.Expr.OpCall.Params(),
            ),
          ),
          lang.Expr(lang.Expr.StringLiteral("y")),
        ),
      ),
    )
    assertEquals(
      "x[1, 2, 3]".parseStr,
      Node.Top(
        lang.Expr.FnCall(
          lang.Expr(
            lang.Expr.OpCall(
              lang.Id("x"),
              lang.Expr.OpCall.Params(),
            ),
          ),
          lang.Expr(
            lang.Expr.TupleLiteral(
              lang.Expr(lang.Expr.NumberLiteral("1")),
              lang.Expr(lang.Expr.NumberLiteral("2")),
              lang.Expr(lang.Expr.NumberLiteral("3")),
            ),
          ),
        ),
      ),
    )

  test("If"):
    assertEquals(
      """IF A 
        |THEN 1
        |ELSE 2""".stripMargin.parseStr,
      Node.Top(
        lang.Expr.If(
          lang.Expr(
            lang.Expr.OpCall(
              lang.Id("A"),
              lang.Expr.OpCall.Params(),
            ),
          ),
          lang.Expr(lang.Expr.NumberLiteral("1")),
          lang.Expr(lang.Expr.NumberLiteral("2")),
        ),
      ),
    )

  test("Case"):
    assertEquals(
      """CASE A -> 1 
        | [] B -> 2""".stripMargin.parseStr,
      Node.Top(
        lang.Expr.Case(
          lang.Expr.Case.Branches(
            lang.Expr.Case.Branch(
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id("A"),
                  lang.Expr.OpCall.Params(),
                ),
              ),
              lang.Expr(lang.Expr.NumberLiteral("1")),
            ),
            lang.Expr.Case.Branch(
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id("B"),
                  lang.Expr.OpCall.Params(),
                ),
              ),
              lang.Expr(lang.Expr.NumberLiteral("2")),
            ),
          ),
          lang.Expr.Case.Other(lang.Expr.Case.Other.None()),
        ),
      ),
    )
    assertEquals(
      """CASE A -> 1 
        | [] B -> 2
        | OTHER -> 3""".stripMargin.parseStr,
      Node.Top(
        lang.Expr.Case(
          lang.Expr.Case.Branches(
            lang.Expr.Case.Branch(
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id("A"),
                  lang.Expr.OpCall.Params(),
                ),
              ),
              lang.Expr(lang.Expr.NumberLiteral("1")),
            ),
            lang.Expr.Case.Branch(
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id("B"),
                  lang.Expr.OpCall.Params(),
                ),
              ),
              lang.Expr(lang.Expr.NumberLiteral("2")),
            ),
          ),
          lang.Expr.Case.Other(
            lang.Expr(lang.Expr.NumberLiteral("3")),
          ),
        ),
      ),
    )

  test("LET"):
    assertEquals(
      """LET x == 1
        |y == 2
        |IN  {y, x}""".stripMargin.parseStr,
      Node.Top(
        lang.Expr.Let(
          lang.Expr.Let.Defns(
            lang.Operator(
              lang.Id("x"),
              lang.Operator.Params(),
              lang.Expr(lang.Expr.NumberLiteral("1")),
            ),
            lang.Operator(
              lang.Id("y"),
              lang.Operator.Params(),
              lang.Expr(lang.Expr.NumberLiteral("2")),
            ),
          ),
          lang.Expr(
            lang.Expr.SetLiteral(
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id("y"),
                  lang.Expr.OpCall.Params(),
                ),
              ),
              lang.Expr(
                lang.Expr.OpCall(
                  lang.Id("x"),
                  lang.Expr.OpCall.Params(),
                ),
              ),
            ),
          ),
        ),
      ),
    )

  test("Exists"):
    assertEquals(
      "\\E x \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        lang.Expr.Exists(
          lang.QuantifierBounds(
            lang.QuantifierBound(
              lang.Id("x"),
              lang.Expr(
                lang.Expr.SetLiteral(
                  lang.Expr(lang.Expr.NumberLiteral("1")),
                  lang.Expr(lang.Expr.NumberLiteral("2")),
                  lang.Expr(lang.Expr.NumberLiteral("3")),
                ),
              ),
            ),
          ),
          lang.Expr(
            lang.Expr.OpCall(
              lang.OpSym(defns.`=`("=")),
              lang.Expr.OpCall.Params(
                lang.Expr(
                  lang.Expr.OpCall(
                    lang.Id("x"),
                    lang.Expr.OpCall.Params(),
                  ),
                ),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(
      "\\E <<x, y, z>> \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        lang.Expr.Exists(
          lang.QuantifierBounds(
            lang.QuantifierBound(
              lang.Ids(
                lang.Id("x"),
                lang.Id("y"),
                lang.Id("z"),
              ),
              lang.Expr(
                lang.Expr.SetLiteral(
                  lang.Expr(lang.Expr.NumberLiteral("1")),
                  lang.Expr(lang.Expr.NumberLiteral("2")),
                  lang.Expr(lang.Expr.NumberLiteral("3")),
                ),
              ),
            ),
          ),
          lang.Expr(
            lang.Expr.OpCall(
              lang.OpSym(defns.`=`("=")),
              lang.Expr.OpCall.Params(
                lang.Expr(
                  lang.Expr.OpCall(
                    lang.Id("x"),
                    lang.Expr.OpCall.Params(),
                  ),
                ),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
          ),
        ),
      ),
    )
    assertEquals(
      "\\E x \\in {1, 2, 3}, y \\in {4, 5, 6} : x = 2".parseStr,
      Node.Top(
        lang.Expr.Exists(
          lang.QuantifierBounds(
            lang.QuantifierBound(
              lang.Id("x"),
              lang.Expr(
                lang.Expr.SetLiteral(
                  lang.Expr(lang.Expr.NumberLiteral("1")),
                  lang.Expr(lang.Expr.NumberLiteral("2")),
                  lang.Expr(lang.Expr.NumberLiteral("3")),
                ),
              ),
            ),
            lang.QuantifierBound(
              lang.Id("y"),
              lang.Expr(
                lang.Expr.SetLiteral(
                  lang.Expr(lang.Expr.NumberLiteral("4")),
                  lang.Expr(lang.Expr.NumberLiteral("5")),
                  lang.Expr(lang.Expr.NumberLiteral("6")),
                ),
              ),
            ),
          ),
          lang.Expr(
            lang.Expr.OpCall(
              lang.OpSym(defns.`=`("=")),
              lang.Expr.OpCall.Params(
                lang.Expr(
                  lang.Expr.OpCall(
                    lang.Id("x"),
                    lang.Expr.OpCall.Params(),
                  ),
                ),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
          ),
        ),
      ),
    )

  test("Forall"):
    assertEquals(
      "\\A x \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        lang.Expr.Forall(
          lang.QuantifierBounds(
            lang.QuantifierBound(
              lang.Id("x"),
              lang.Expr(
                lang.Expr.SetLiteral(
                  lang.Expr(lang.Expr.NumberLiteral("1")),
                  lang.Expr(lang.Expr.NumberLiteral("2")),
                  lang.Expr(lang.Expr.NumberLiteral("3")),
                ),
              ),
            ),
          ),
          lang.Expr(
            lang.Expr.OpCall(
              lang.OpSym(defns.`=`("=")),
              lang.Expr.OpCall.Params(
                lang.Expr(
                  lang.Expr.OpCall(
                    lang.Id("x"),
                    lang.Expr.OpCall.Params(),
                  ),
                ),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
          ),
        ),
      ),
    )

  // TODO: AA EE ??

  test("Temporal Logic Combined"):
    assertEquals(
      "s /\\ \\E x \\in y : z /\\ \\E p \\in q : r".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(defns./\("/\\")),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.Id("s"),
                lang.Expr.OpCall.Params(),
              ),
            ),
            lang.Expr(
              lang.Expr.Exists(
                lang.QuantifierBounds(
                  lang.QuantifierBound(
                    lang.Id("x"),
                    lang.Expr(
                      lang.Expr.OpCall(
                        lang.Id("y"),
                        lang.Expr.OpCall.Params(),
                      ),
                    ),
                  ),
                ),
                lang.Expr(
                  lang.Expr.OpCall(
                    lang.OpSym(defns./\("/\\")),
                    lang.Expr.OpCall.Params(
                      lang.Expr(
                        lang.Expr.OpCall(
                          lang.Id("z"),
                          lang.Expr.OpCall.Params(),
                        ),
                      ),
                      lang.Expr(
                        lang.Expr.Exists(
                          lang.QuantifierBounds(
                            lang.QuantifierBound(
                              lang.Id("p"),
                              lang.Expr(
                                lang.Expr.OpCall(
                                  lang.Id("q"),
                                  lang.Expr.OpCall.Params(),
                                ),
                              ),
                            ),
                          ),
                          lang.Expr(
                            lang.Expr.OpCall(
                              lang.Id("r"),
                              lang.Expr.OpCall.Params(),
                            ),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )

  // todo: function
  // todo: set comprehension
  // todo: set refinement
  // todo: lambda

  test("Choose"):
    assertEquals(
      "CHOOSE x \\in {1, 2, 3} : x = 2".parseStr,
      Node.Top(
        lang.Expr.Choose(
          lang.QuantifierBound(
            lang.Id("x"),
            lang.Expr(
              lang.Expr.SetLiteral(
                lang.Expr(lang.Expr.NumberLiteral("1")),
                lang.Expr(lang.Expr.NumberLiteral("2")),
                lang.Expr(lang.Expr.NumberLiteral("3")),
              ),
            ),
          ),
          lang.Expr(
            lang.Expr.OpCall(
              lang.OpSym(defns.`=`("=")),
              lang.Expr.OpCall.Params(
                lang.Expr(
                  lang.Expr.OpCall(
                    lang.Id("x"),
                    lang.Expr.OpCall.Params(),
                  ),
                ),
                lang.Expr(lang.Expr.NumberLiteral("2")),
              ),
            ),
          ),
        ),
      ),
    )
    // todo: id nil, tuple expr, tuple nil

  // todo: except

  test("Parentheses"):
    assertEquals(
      "(1)".parseStr,
      Node.Top(lang.Expr.NumberLiteral("1")),
    )
    assertEquals(
      "(((x)))".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.Id("x"),
          lang.Expr.OpCall.Params(),
        ),
      ),
    )
    assertEquals(
      "(5 + 6)".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(
            defns.+("+"),
          ),
          lang.Expr.OpCall.Params(
            lang.Expr(lang.Expr.NumberLiteral("5")),
            lang.Expr(lang.Expr.NumberLiteral("6")),
          ),
        ),
      ),
    )
    assertEquals(
      "(1 + 2) * 3".parseStr,
      Node.Top(
        lang.Expr.OpCall(
          lang.OpSym(
            defns.*("*"),
          ),
          lang.Expr.OpCall.Params(
            lang.Expr(
              lang.Expr.OpCall(
                lang.OpSym(
                  defns.+("+"),
                ),
                lang.Expr.OpCall.Params(
                  lang.Expr(lang.Expr.NumberLiteral("1")),
                  lang.Expr(lang.Expr.NumberLiteral("2")),
                ),
              ),
            ),
            lang.Expr(lang.Expr.NumberLiteral("3")),
          ),
        ),
      ),
    )
    // assertEquals(
    //   "\\A x \\in {1, 2, 3} : x = (2)".parseStr,
    //   Node.Top(
    //     lang.Expr.Forall(
    //       lang.QuantifierBounds(
    //         lang.QuantifierBound(
    //           lang.Id("x"),
    //           lang.Expr(lang.Expr.SetLiteral(
    //             lang.Expr(lang.Expr.NumberLiteral("1")),
    //             lang.Expr(lang.Expr.NumberLiteral("2")),
    //             lang.Expr(lang.Expr.NumberLiteral("3"))
    //           )))),
    //       lang.Expr(lang.Expr.OpCall(
    //         lang.OpSym(defns.`=`("=")),
    //         lang.Expr.OpCall.Params(
    //           lang.Expr(lang.Expr.OpCall(
    //             lang.Id("x"),
    //             lang.Expr.OpCall.Params()
    //           )),
    //           lang.Expr(lang.Expr.NumberLiteral("2"))
    //         ))))))

  // test("Conjunctions"):
  //   assertEquals(
  //     "/\\ 1".parseStr,
  //     Node.Top(
  //       lang.Expr.NumberLiteral("1")
  //     ))
  //   assertEquals(
  //     "/\\ 1 \\/ 2".parseStr,
  //     Node.Top(
  //       lang.Expr.OpCall(
  //         lang.OpSym(defns.\/("\\/")),
  //         lang.Expr.OpCall.Params(
  //           lang.Expr(lang.Expr.NumberLiteral("1")),
  //           lang.Expr(lang.Expr.NumberLiteral("2"))
  //         ))))
  //   assertEquals(
  //     s"""
  //     |/\\ 1
  //     |/\\ 2
  //     |""".stripMargin.parseStr,
  //     Node.Top(
  //       lang.Expr.OpCall(
  //         lang.OpSym(defns./\("/\\")),
  //         lang.Expr.OpCall.Params(
  //           lang.Expr(lang.Expr.NumberLiteral("1")),
  //           lang.Expr(lang.Expr.NumberLiteral("2"))
  //       ))))
  //   assertEquals(
  //     s"""
  //     |/\\ 1 \\/ 1
  //     |/\\ 2 \\/ 2
  //     |/\\ 3 \\/ 3
  //     |""".stripMargin.parseStr,
  //     Node.Top(lang.Expr.OpCall(
  //       lang.OpSym(defns./\("/\\")),
  //       lang.Expr.OpCall.Params(
  //         lang.Expr(lang.Expr.OpCall(
  //             lang.OpSym(defns.\/("\\/")),
  //             lang.Expr.OpCall.Params(
  //               lang.Expr(lang.Expr.NumberLiteral("1")),
  //               lang.Expr(lang.Expr.NumberLiteral("1"))
  //             )
  //         )),
  //         lang.Expr(lang.Expr.OpCall(
  //             lang.OpSym(defns./\("/\\")),
  //             lang.Expr.OpCall.Params(
  //               lang.Expr(lang.Expr.OpCall(
  //                 lang.OpSym(defns.\/("\\/")),
  //                 lang.Expr.OpCall.Params(
  //                   lang.Expr(lang.Expr.NumberLiteral("2")),
  //                   lang.Expr(lang.Expr.NumberLiteral("2"))
  //                 ))),
  //               lang.Expr(lang.Expr.OpCall(
  //                 lang.OpSym(defns.\/("\\/")),
  //                 lang.Expr.OpCall.Params(
  //                   lang.Expr(lang.Expr.NumberLiteral("3")),
  //                   lang.Expr(lang.Expr.NumberLiteral("3"))
  //                 ))))))))))

  // assertEquals(
  //   s"""
  //   |/\\ 1
  //   | /\\ 2
  //   |/\\ 3
  //   |""".stripMargin.parseStr,
  //   Node.Top(
  //     lang.Expr.OpCall(
  //       lang.OpSym(defns./\("/\\")),
  //       lang.Expr.OpCall.Params(
  //         lang.Expr(lang.Expr.OpCall(
  //           lang.OpSym(defns./\("/\\")),
  //           lang.Expr.OpCall.Params(
  //             lang.Expr(lang.Expr.NumberLiteral("1")),
  //             lang.Expr(lang.Expr.NumberLiteral("2"))
  //           )
  //         )),
  //         lang.Expr(lang.Expr.NumberLiteral("3"))
  //       )
  //     )
  //   ))

  // assertEquals(
  //   s"""
  //   |/\\ \\E x \in {1, 2, 3} : 1
  //   | /\\ 2
  //   |""".stripMargin.parseStr,
  //   Node.Top(
  //     lang.Expr.NumberLiteral("1")
  //   ))

  // assertEquals(
  //   s"""
  //   |/\\ \\E x \in {1, 2, 3} : 1
  //   |/\\ 2
  //   |""".stripMargin.parseStr,
  //   Node.Top(
  //     lang.Expr.NumberLiteral("1")
  //   ))

  // assertEquals(
  //   s"""
  //   | /\\ \\E x \in {1, 2, 3} : 1
  //   |/\\ 2
  //   |""".stripMargin.parseStr,
  //   Node.Top(
  //     lang.Expr.NumberLiteral("1")
  //   ))

  // assertEquals(
  //   s"""
  //   |/\\ 1
  //   |  /\\ 2
  //   |  /\\ 3
  //   | /\\ 4
  //   |/\\ 5
  //   | /\\ 6
  //   |""".stripMargin.parseStr,
  //   Node.Top(
  //     lang.Expr.NumberLiteral("1")
  //   ))

// TODO
// paren
// conjucntion
// lamda function set
