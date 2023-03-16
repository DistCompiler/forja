package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class IRBuilderHelpersTest extends AnyFunSuite {
  final case class AssignPairsTest(ctx: IRBuilder.Context,
                                   input: DCalAST.Statement.AssignPairs,
                                   expectedOutput: List[IR.Node])

  List(
    AssignPairsTest(
      ctx = IRBuilder.Context(
        stateName = "_state1",
        nameInfoOf = Map[String, IRBuilder.NameInfo](
          "str" -> IRBuilder.NameInfo.State
        )
      ),
      input = DCalAST.Statement.AssignPairs(
        assignPairs = List(
          DCalAST.AssignPair(
            name = "str",
            expression = DCalAST.Expression.StringLiteral("new string")
          )
        )
      ),
      expectedOutput = List(
        IR.Node.MapOnSet(
          set = List(IR.Node.Name("_state1")),
          setMember = "l1",
          proc = List(
            IR.Node.Uninterpreted("["),
            IR.Node.Name("l1"),
            IR.Node.Uninterpreted(" EXCEPT "),
            IR.Node.Uninterpreted("!.str = "),
            IR.Node.Uninterpreted(""""new string""""),
            IR.Node.Uninterpreted("]")
          )
        )
      )
    ),
    AssignPairsTest(
      ctx = IRBuilder.Context(
        stateName = "_state1",
        nameInfoOf = Map[String, IRBuilder.NameInfo](
          "y" -> IRBuilder.NameInfo.State,
          "x" -> IRBuilder.NameInfo.State
        )
      ),
      input = DCalAST.Statement.AssignPairs(
        assignPairs = List(
          DCalAST.AssignPair(
            name = "y",
            expression = DCalAST.Expression.ExpressionBinOp(
              lhs = DCalAST.Expression.Name("y"),
              binOp = DCalAST.BinOp.Minus,
              rhs = DCalAST.Expression.IntLiteral(1)
            )
          ),
          DCalAST.AssignPair(
            name = "x",
            expression = DCalAST.Expression.ExpressionBinOp(
              lhs = DCalAST.Expression.Name("x"),
              binOp = DCalAST.BinOp.Plus,
              rhs = DCalAST.Expression.IntLiteral(1)
            )
          ),
        )
      ),
      expectedOutput = List(
        IR.Node.MapOnSet(
          set = List(IR.Node.Name("_state1")),
          setMember = "l2",
          proc = List(
            IR.Node.Uninterpreted("["),
            IR.Node.Name("l2"),
            IR.Node.Uninterpreted(" EXCEPT "),
            IR.Node.Uninterpreted("!.y = "),
            IR.Node.Name("l2"),
            IR.Node.Uninterpreted(".y"),
            IR.Node.Uninterpreted(" - "),
            IR.Node.Uninterpreted("1"),
            IR.Node.Uninterpreted(", "),
            IR.Node.Uninterpreted("!.x = "),
            IR.Node.Name("l2"),
            IR.Node.Uninterpreted(".x"),
            IR.Node.Uninterpreted(" + "),
            IR.Node.Uninterpreted("1"),
            IR.Node.Uninterpreted("]")
          )
        )
      )
    )
  ).foreach {
    case AssignPairsTest(ctx, input, expectedOutput) =>
      test(s"generateAssignPairs($input)") {
        val actualOutput = IRBuilder.generateAssignPairs(input)(using ctx)
        assert(actualOutput == expectedOutput)
      }
  }
}
