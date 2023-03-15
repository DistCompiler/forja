package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class DCalTokenizerTest extends AnyFunSuite {

  import DCalTokenizer.*

  def pos(line: Int, column: Int): Position =
    Position(fileName = "<testfile>", line = line, column = column)

  List(
    "12" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = DCalTokenData.IntLiteral(BigInt(12))
      ),
    ),
    "12 " -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = DCalTokenData.IntLiteral(BigInt(12))
      ),
    ),
    "12\n13" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = DCalTokenData.IntLiteral(BigInt(12))
      ),
      Token(
        startPosition = pos(2, 1),
        endPosition = pos(2, 2),
        data = DCalTokenData.IntLiteral(BigInt(13))
      ),
    ),
    """"p"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, """"p"""".length),
        data = DCalTokenData.StringLiteral("p")
      )
    ),
    """"one string"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, """"one string"""".length),
        data = DCalTokenData.StringLiteral("one string")
      )
    ),
    """"first line\nsecond line"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, """"first line\nsecond line"""".length),
        data = DCalTokenData.StringLiteral("first line\nsecond line")
      )
    ),
    "_pv1" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, "_pv1".length),
        data = DCalTokenData.Name("_pv1")
      )
    ),
    "TRUE" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 4),
        data = DCalTokenData.True
      )
    ),
    "let x = 19 + y" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 3),
        data = DCalTokenData.Let
      ),
      Token(
        startPosition = pos(1, 5),
        endPosition = pos(1, 5),
        data = DCalTokenData.Name("x")
      ),
      Token(
        startPosition = pos(1, 7),
        endPosition = pos(1, 7),
        data = DCalTokenData.EqualTo
      ),
      Token(
        startPosition = pos(1, 9),
        endPosition = pos(1, 10),
        data = DCalTokenData.IntLiteral(19)
      ),
      Token(
        startPosition = pos(1, 12),
        endPosition = pos(1, 12),
        data = DCalTokenData.Plus
      ),
      Token(
        startPosition = pos(1, 14),
        endPosition = pos(1, 14),
        data = DCalTokenData.Name("y")
      )
    ),
    "var i \\in lst" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 3),
        data = DCalTokenData.Var
      ),
      Token(
        startPosition = pos(1, 5),
        endPosition = pos(1, 5),
        data = DCalTokenData.Name("i")
      ),
      Token(
        startPosition = pos(1, 7),
        endPosition = pos(1, 9),
        data = DCalTokenData.SlashIn
      ),
      Token(
        startPosition = pos(1, 11),
        endPosition = pos(1, 13),
        data = DCalTokenData.Name("lst")
      )
    ),
    "if x <= y then z := FALSE" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = DCalTokenData.If
      ),
      Token(
        startPosition = pos(1, 4),
        endPosition = pos(1, 4),
        data = DCalTokenData.Name("x")
      ),
      Token(
        startPosition = pos(1, 6),
        endPosition = pos(1, 7),
        data = DCalTokenData.LesserThanOrEqualTo
      ),
      Token(
        startPosition = pos(1, 9),
        endPosition = pos(1, 9),
        data = DCalTokenData.Name("y")
      ),
      Token(
        startPosition = pos(1, 11),
        endPosition = pos(1, 14),
        data = DCalTokenData.Then
      ),
      Token(
        startPosition = pos(1, 16),
        endPosition = pos(1, 16),
        data = DCalTokenData.Name("z")
      ),
      Token(
        startPosition = pos(1, 18),
        endPosition = pos(1, 19),
        data = DCalTokenData.Walrus
      ),
      Token(
        startPosition = pos(1, 21),
        endPosition = pos(1, 25),
        data = DCalTokenData.False
      )
    ),
    """y := "new_y"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 1),
        data = DCalTokenData.Name("y")
      ),
      Token(
        startPosition = pos(1, 3),
        endPosition = pos(1, 4),
        data = DCalTokenData.Walrus
      ),
      Token(
        startPosition = pos(1, 6),
        endPosition = pos(1, 12),
        data = DCalTokenData.StringLiteral("new_y")
      )
    ),
    "z1 := z2 || z2 := z1" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = DCalTokenData.Name("z1")
      ),
      Token(
        startPosition = pos(1, 4),
        endPosition = pos(1, 5),
        data = DCalTokenData.Walrus
      ),
      Token(
        startPosition = pos(1, 7),
        endPosition = pos(1, 8),
        data = DCalTokenData.Name("z2")
      ),
      Token(
        startPosition = pos(1, 10),
        endPosition = pos(1, 11),
        data = DCalTokenData.DoublePipe
      ),
      Token(
        startPosition = pos(1, 13),
        endPosition = pos(1, 14),
        data = DCalTokenData.Name("z2")
      ),
      Token(
        startPosition = pos(1, 16),
        endPosition = pos(1, 17),
        data = DCalTokenData.Walrus
      ),
      Token(
        startPosition = pos(1, 19),
        endPosition = pos(1, 20),
        data = DCalTokenData.Name("z1")
      ),
    ),
    "def wait() {await lockRelease}" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 3),
        data = DCalTokenData.Def
      ),
      Token(
        startPosition = pos(1, 5),
        endPosition = pos(1, 8),
        data = DCalTokenData.Name("wait")
      ),
      Token(
        startPosition = pos(1, 9),
        endPosition = pos(1, 9),
        data = DCalTokenData.OpenParenthesis
      ),
      Token(
        startPosition = pos(1, 10),
        endPosition = pos(1, 10),
        data = DCalTokenData.CloseParenthesis
      ),
      Token(
        startPosition = pos(1, 12),
        endPosition = pos(1, 12),
        data = DCalTokenData.OpenCurlyBracket
      ),
      Token(
        startPosition = pos(1, 13),
        endPosition = pos(1, 17),
        data = DCalTokenData.Await
      ),
      Token(
        startPosition = pos(1, 19),
        endPosition = pos(1, 29),
        data = DCalTokenData.Name("lockRelease")
      ),
      Token(
        startPosition = pos(1, 30),
        endPosition = pos(1, 30),
        data = DCalTokenData.CloseCurlyBracket
      )
    ),
    "import ModuleA, ModuleB, ModuleC" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 6),
        data = DCalTokenData.Import
      ),
      Token(
        startPosition = pos(1, 8),
        endPosition = pos(1, 14),
        data = DCalTokenData.Name("ModuleA")
      ),
      Token(
        startPosition = pos(1, 15),
        endPosition = pos(1, 15),
        data = DCalTokenData.Comma
      ),
      Token(
        startPosition = pos(1, 17),
        endPosition = pos(1, 23),
        data = DCalTokenData.Name("ModuleB")
      ),
      Token(
        startPosition = pos(1, 24),
        endPosition = pos(1, 24),
        data = DCalTokenData.Comma
      ),
      Token(
        startPosition = pos(1, 26),
        endPosition = pos(1, 32),
        data = DCalTokenData.Name("ModuleC")
      )
    )
  ).foreach {
    case (input, expectedResult) =>
      test(s"tokenize($input)") {
        val actualResult = DCalTokenizer(
          contents = input,
          fileName = "<testfile>",
        )
          .toList
        assert(actualResult == expectedResult)
      }
  }
}
