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

import forja.*
import forja.dsl.*
import forja.manip.RewriteDebugTracer
import forja.source.{Source, SourceRange}

import Builtin.{Error, SourceMarker}

class CalcTests extends munit.FunSuite:
  extension (str: String)
    def read: Node.Top =
      forja.langs.calc.read
        .fromSourceRange(SourceRange.entire(Source.fromString(str)))

    def parse: Node.Top =
      val top = read

      // format: off
      // instrumentWithTracer(Manip.RewriteDebugTracer(os.pwd / "dbg_calc_parser_passes")):
      CalcParser(top)
      // format: on

      // os.write.over(
      //   os.pwd / "dbg_calc_parser" / "test_output.dbg",
      //   top.toPrettyWritable(CalcReader.wellformed),
      //   createFolders = true
      // )

      top

    def evaluate: Node.Top =
      val top = parse

      // format: off
      instrumentWithTracer(RewriteDebugTracer(os.pwd / "dbg_calc_evaluator_passes")):
        CalcEvaluator(top)
      // format: on

      // os.write.over(
      //   os.pwd / "dbg_calc_evaluator" / "test_output.dbg",
      //   top.toPrettyWritable(CalcReader.wellformed),
      //   createFolders = true
      // )

      top

  test("empty string"):
    assertEquals("".read, Node.Top())

  test("only whitespace"):
    assertEquals("    \n\t".read, Node.Top())

  test("error: invalid character"):
    assertEquals("k".read, Node.Top(Error("invalid byte", SourceMarker("k"))))

  test("single number"):
    assertEquals(
      "5".read,
      Node.Top(
        lang.Expression(
          lang.Number("5"),
        ),
      ),
    )

  test("read basic addition"):
    assertEquals(
      "5 + 11".read,
      Node.Top(
        lang.Expression(
          lang.Number("5"),
        ),
        CalcReader.AddOp("+"),
        lang.Expression(
          lang.Number("11"),
        ),
      ),
    )

  test("read grouped"):
    assertEquals(
      "(2 + 3) * 4".read,
      Node.Top(
        CalcReader.Group(
          lang.Expression(lang.Number("2")),
          CalcReader.AddOp("+"),
          lang.Expression(lang.Number("3")),
        ),
        CalcReader.MulOp("*"),
        lang.Expression(lang.Number("4")),
      ),
    )

  test("read grouped grouped"):
    assertEquals(
      "((2 + 3) * 4)".read,
      Node.Top(
        CalcReader.Group(
          CalcReader.Group(
            lang.Expression(lang.Number("2")),
            CalcReader.AddOp("+"),
            lang.Expression(lang.Number("3")),
          ),
          CalcReader.MulOp("*"),
          lang.Expression(lang.Number("4")),
        ),
      ),
    )

  test("read basic multiplication"):
    assertEquals(
      "5 * 11".read,
      Node.Top(
        lang.Expression(
          lang.Number("5"),
        ),
        CalcReader.MulOp("*"),
        lang.Expression(
          lang.Number("11"),
        ),
      ),
    )

  test("read full calculation"):
    assertEquals(
      "5 + 11 * 4".read,
      Node.Top(
        lang.Expression(
          lang.Number("5"),
        ),
        CalcReader.AddOp("+"),
        lang.Expression(
          lang.Number("11"),
        ),
        CalcReader.MulOp("*"),
        lang.Expression(
          lang.Number("4"),
        ),
      ),
    )

  test("simple addition parse"):
    assertEquals(
      "5 + 11".parse,
      Node.Top(
        lang
          .Expression(
            lang
              .Add(
                lang.Expression(
                  lang.Number("5"),
                ),
                lang.Expression(
                  lang.Number("11"),
                ),
              )
              .at("5 + 11"),
          )
          .at("5 + 11"),
      ),
    )

  test("simple multiplication parse"):
    assertEquals(
      "5 * 11".parse,
      Node.Top(
        lang
          .Expression(
            lang
              .Mul(
                lang.Expression(
                  lang.Number("5"),
                ),
                lang.Expression(
                  lang.Number("11"),
                ),
              )
              .at("5 * 11"),
          )
          .at("5 * 11"),
      ),
    )

  test("full calculation parse"):
    assertEquals(
      "5 + 11 * 4".parse,
      Node.Top(
        lang
          .Expression(
            lang
              .Add(
                lang.Expression(
                  lang.Number("5"),
                ),
                lang
                  .Expression(
                    lang
                      .Mul(
                        lang.Expression(
                          lang.Number("11"),
                        ),
                        lang.Expression(
                          lang.Number("4"),
                        ),
                      )
                      .at("11 * 4"),
                  )
                  .at("11 * 4"),
              )
              .at("5 + 11 * 4"),
          )
          .at("5 + 11 * 4"),
      ),
    )

  test("grouped parse"):
    assertEquals(
      "((2 + 3) * 4)".parse,
      Node.Top(
        lang.Expression(
          lang.Mul(
            lang.Expression(
              lang.Add(
                lang.Expression(lang.Number("2")),
                lang.Expression(lang.Number("3")),
              ),
            ),
            lang.Expression(lang.Number("4")),
          ),
        ),
      ),
    )

  test("full calculation 4 parse"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6 * 2".parse,
      Node.Top(
        lang
          .Expression(
            lang
              .Sub(
                lang
                  .Expression(
                    lang
                      .Add(
                        lang
                          .Expression(
                            lang
                              .Mul(
                                lang.Expression(
                                  lang.Number("5"),
                                ),
                                lang.Expression(
                                  lang.Number("4"),
                                ),
                              )
                              .at("5 * 4"),
                          )
                          .at("5 * 4"),
                        lang
                          .Expression(
                            lang
                              .Div(
                                lang.Expression(
                                  lang.Number("4"),
                                ),
                                lang.Expression(
                                  lang.Number("2"),
                                ),
                              )
                              .at("4 / 2"),
                          )
                          .at("4 / 2"),
                      )
                      .at("5 * 4 + 4 / 2"),
                  )
                  .at("5 * 4 + 4 / 2"),
                lang
                  .Expression(
                    lang
                      .Mul(
                        lang.Expression(
                          lang.Number("6"),
                        ),
                        lang.Expression(
                          lang.Number("2"),
                        ),
                      )
                      .at("6 * 2"),
                  )
                  .at("6 * 2"),
              )
              .at("5 * 4 + 4 / 2 - 6 * 2"),
          )
          .at("5 * 4 + 4 / 2 - 6 * 2"),
      ),
    )

  test("addition calculation"):
    assertEquals(
      "5 + 11".evaluate,
      Node.Top(
        Node.Embed(16),
      ),
    )

  test("multiplication calculation"):
    assertEquals(
      "5 * 11".evaluate,
      Node.Top(
        Node.Embed(55),
      ),
    )

  test("full calculation"):
    assertEquals(
      "5 + 11 * 4".evaluate,
      Node.Top(
        Node.Embed(49),
      ),
    )

  test("full calculation 2"):
    assertEquals(
      "5 * 4 + 4 / 2".evaluate,
      Node.Top(
        Node.Embed(22),
      ),
    )

  test("full calculation 3"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6".evaluate,
      Node.Top(
        Node.Embed(16),
      ),
    )

  test("full calculation 4"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6 * 2".evaluate,
      Node.Top(
        Node.Embed(10),
      ),
    )
