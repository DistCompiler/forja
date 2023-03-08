package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestIRBuilderHelpers extends AnyFunSuite {
  final case class GenerateAssignPairsTest(ctx: IRBuilder.Context,
                                           input: List[DCalAST.AssignPair],
                                           expectedOutput: List[IR.Node])

  List(
    GenerateAssignPairsTest(
      ctx = IRBuilder.Context(
        state = 1,
        nameInfoOf = Map[String, IRBuilder.NameInfo](
          "str" -> IRBuilder.NameInfo.State
        )
      ),
      input = List(
        DCalAST.AssignPair(
          name = "str",
          expression = DCalAST.Expression.StringLiteral("new string")
        )
      ),
      expectedOutput = List(
        IR.Node.MapOnSet(
          set = List(IR.Node.Name("_state1")),
          setMember = "s",
          proc = List(
            IR.Node.Uninterpreted("["),
            IR.Node.Name("s"),
            IR.Node.Uninterpreted(" EXCEPT "),
            IR.Node.Uninterpreted("!.str = "),
            IR.Node.Uninterpreted(""""new string""""),
            IR.Node.Uninterpreted("]")
          )
        )
      )
    ),
    GenerateAssignPairsTest(
      ctx = IRBuilder.Context(
        state = 1,
        nameInfoOf = Map[String, IRBuilder.NameInfo](
          "y" -> IRBuilder.NameInfo.State,
          "x" -> IRBuilder.NameInfo.State
        )
      ),
      input = List(
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
      ),
      expectedOutput = List(
        IR.Node.MapOnSet(
          set = List(IR.Node.Name("_state1")),
          setMember = "s",
          proc = List(
            IR.Node.Uninterpreted("["),
            IR.Node.Name("s"),
            IR.Node.Uninterpreted(" EXCEPT "),
            IR.Node.Uninterpreted("!.y = "),
            IR.Node.Name("s"),
            IR.Node.Uninterpreted(".y"),
            IR.Node.Uninterpreted(" - "),
            IR.Node.Uninterpreted("1"),
            IR.Node.Uninterpreted(", "),
            IR.Node.Uninterpreted("!.x = "),
            IR.Node.Name("s"),
            IR.Node.Uninterpreted(".x"),
            IR.Node.Uninterpreted(" + "),
            IR.Node.Uninterpreted("1"),
            IR.Node.Uninterpreted("]")
          )
        )
      )
    )
  ).foreach {
    case GenerateAssignPairsTest(ctx, input, expectedOutput) =>
      test(s"generateAssignPairs($input)") {
        val actualOutput = IRBuilder.generateAssignPairs(input)(using ctx)
        assert(actualOutput == expectedOutput)
      }
  }
}
