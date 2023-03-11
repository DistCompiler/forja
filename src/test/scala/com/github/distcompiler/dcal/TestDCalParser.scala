package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.TestUtils
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable
import scala.collection.immutable.List

class TestDCalParser extends AnyFunSuite {

  import DCalParser.*

  val testModule = "module TestModule1"
  val testImports = "import TestModule2"

  val testDefNoParamsNoBody = "def aFunc1() {}"
  val expectedDefNoParamsNoBody = DCalAST.Definition(
    name = "aFunc1",
    params = Nil,
    body = DCalAST.Block(statements = Nil)
  )

  val testDefParamsNoBody = "def aFunc2(p1, p2, p3) {}"
  val expectedDefParamsNoBody = DCalAST.Definition(
    name = "aFunc2",
    params = List("p1", "p2", "p3"),
    body = DCalAST.Block(statements = Nil)
  )

  val testLet = "let test1 = TRUE;"
  val testVar = "var test2;"
  val testVarEquals = """var test3 = "val3";"""
  val testVarSlashIn = "var test4 \\in { 1, 2, 3, 4, 5 };"
  val testIf = "if x <= y then { x := x + 1; } else { y := y - 1; }"
  val testBracketedExpression = "(test6)"
  val testAssignPairs = s"test6 := test7 || test7 := $testBracketedExpression;"
  val testExpression = "test6 > 1000"
  val testAwait = s"await $testExpression;"
  val testDefParamsBody = s"def aFunc(anArg) {\n${
    TestUtils.sequenceLines(
      testLet, testVar, testVarEquals, testVarSlashIn, testAssignPairs, testAwait, testIf
    )
  }\n}"

  List(
    testModule -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = Nil
    ),
    TestUtils.sequenceLines(testModule, testImports) -> DCalAST.Module(
      name = "TestModule1",
      imports = List("TestModule2"),
      definitions = Nil
    ),
    TestUtils.sequenceLines(testModule, testDefNoParamsNoBody) -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(
        expectedDefNoParamsNoBody
      )
    ),
    TestUtils.sequenceLines(testModule, testDefParamsBody) -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(
        DCalAST.Definition(
          name = "aFunc",
          params = List("anArg"),
          body = DCalAST.Block(
            statements = List(
              DCalAST.Statement.Let(name = "test1", expression = DCalAST.Expression.True),
              DCalAST.Statement.Var(name = "test2", expressionOpt = None),
              DCalAST.Statement.Var(
                name = "test3",
                expressionOpt = Some((DCalAST.RelOp.EqualTo, DCalAST.Expression.StringLiteral("val3")))
              ),
              DCalAST.Statement.Var(
                name = "test4",
                expressionOpt = Some(
                  (
                    DCalAST.BinOp.SlashIn,
                    DCalAST.Expression.Set(
                      members = List(
                        DCalAST.Expression.IntLiteral(1),
                        DCalAST.Expression.IntLiteral(2),
                        DCalAST.Expression.IntLiteral(3),
                        DCalAST.Expression.IntLiteral(4),
                        DCalAST.Expression.IntLiteral(5),
                      )
                    )
                  )
                )),
              DCalAST.Statement.AssignPairs(
                assignPairs = List(
                  DCalAST.AssignPair(name = "test6", expression = DCalAST.Expression.Name("test7")),
                  DCalAST.AssignPair(name = "test7", expression = DCalAST.Expression.Name("test6"))
                )
              ),
              DCalAST.Statement.Await(
                expression = DCalAST.Expression.ExpressionRelOp(
                  lhs = DCalAST.Expression.Name("test6"),
                  relOp = DCalAST.RelOp.GreaterThan,
                  rhs = DCalAST.Expression.IntLiteral(1000)
                )
              ),
              DCalAST.Statement.IfThenElse(
                predicate = DCalAST.Expression.ExpressionRelOp(
                  lhs = DCalAST.Expression.Name("x"),
                  relOp = DCalAST.RelOp.LesserThanOrEqualTo,
                  rhs = DCalAST.Expression.Name("y")
                ),
                thenBlock = DCalAST.Block(
                  statements = List(
                    DCalAST.Statement.AssignPairs(
                      assignPairs = List(
                        DCalAST.AssignPair(
                          name = "x",
                          expression = DCalAST.Expression.ExpressionBinOp(
                            lhs = DCalAST.Expression.Name("x"),
                            binOp = DCalAST.BinOp.Plus,
                            rhs = DCalAST.Expression.IntLiteral(1)
                          )
                        )
                      )
                    )
                  )
                ),
                elseBlock = DCalAST.Block(
                  statements = List(
                    DCalAST.Statement.AssignPairs(
                      assignPairs = List(
                        DCalAST.AssignPair(
                          name = "y",
                          expression = DCalAST.Expression.ExpressionBinOp(
                            lhs = DCalAST.Expression.Name("y"),
                            binOp = DCalAST.BinOp.Minus,
                            rhs = DCalAST.Expression.IntLiteral(1)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    TestUtils.sequenceLines(testModule, testDefNoParamsNoBody, testDefParamsNoBody) -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(expectedDefNoParamsNoBody, expectedDefParamsNoBody)
    )
  ).foreach {
    case (input, expectedResult) =>
      test(s"parse($input)") {
        val actualResult = DCalParser(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualResult == expectedResult)
      }
  }
}
