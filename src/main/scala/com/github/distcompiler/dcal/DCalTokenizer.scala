package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalTokenizer.tokenize.rep
import com.github.distcompiler.dcal.TokenData.{CloseCurlyBracket, OpenCurlyBracket}

import scala.collection.View
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input
import scala.util.parsing.input.{NoPosition, Reader}

object DCalTokenizer {
  final case class Position(fileName: String, line: Int, column: Int) extends input.Position {
    protected override def lineContents: String = ??? // should be unreachable
  }

  /**
   * A token decorated with position info
   *
   * @param startPosition token starting line and column
   * @param endPosition   token ending line and column (inclusive)
   * @param data          token data, one of TokenData enum
   */
  final case class Token(startPosition: Position, endPosition: Position, data: TokenData)

  class CharsReader(fileName: String, elems: LazyList[Char], line: Int, column: Int, val lastPos: Position)
    extends Reader[Char] {
    override def first: Char = elems.head

    override def rest: Reader[Char] =
      new CharsReader(
        fileName = fileName,
        elems = elems.tail,
        line = first match {
          case '\n' => line + 1
          case _ => line
        },
        column = first match {
          case '\n' => 1
          case _ => column + 1
        },
        lastPos = pos,
      )

    override def atEnd: Boolean = elems.isEmpty

    override def pos: Position =
      Position(fileName = fileName, line = line, column = column)
  }

  object tokenize extends Parsers {
    override type Elem = Char

    def str(str: String): Parser[String] =
      str.view
        .map(elem)
        .reduceOption(_ ~> _)
        .map(_.map(_ => str))
        .getOrElse(success(str))

    def withPosition(dataParser: Parser[TokenData]): Parser[Token] =
      Parser { input =>
        // Our special reader implementation has all the info needed to calculate a position in any situation
        // (even an empty file)
        val startPosition = input.pos.asInstanceOf[Position]
        dataParser(input) match {
          case Success(result, next) =>
            val endPosition = next.asInstanceOf[CharsReader].lastPos
            Success(
              result = Token(startPosition = startPosition, endPosition = endPosition, data = result),
              next = next,
            )
          case noSuccess: NoSuccess => noSuccess
        }
      }

    private val numeric: Parser[Char] = elem("numeric", ch =>
      ch <= '9' && ch >= '0')

    private val alphabetic: Parser[Char] = elem("alphabetic", ch =>
      (ch <= 'z' && ch >= 'a') || (ch <= 'Z' && ch >= 'A'))

    private val intLiteral: Parser[Token] =
      withPosition {
        rep1(numeric).map { digits =>
          TokenData.IntLiteral(BigInt.apply(digits.mkString))
        }
      }

    private val stringLiteral: Parser[Token] =
      withPosition {
        val character: Parser[Char] = elem("character", ch => ch != '"' && ch != '\\') |
          elem('\\') ~> acceptMatch("escape sequence", {
            case '\\' => '\\'
            case '"' => '\"'
            case 'n' => '\n'
            case 't' => '\t'
          })
        (elem('"') ~> rep(character) <~ elem('"'))
          .map(characters => TokenData.StringLiteral(characters.mkString))
      }

    val name: Parser[Token] =
      withPosition {
        val underscore: Parser[Char] = elem('_')
        val character: Parser[Char] = underscore | alphabetic | numeric
        (rep(underscore | numeric) ~ alphabetic ~ rep(character))
          .map{ case c1 ~ c2 ~ c3 => TokenData.Name(s"${c1.mkString}${c2}${c3.mkString}") }
      }

    private val fixedTokens: Parser[Token] =
      List(
        "{" -> TokenData.OpenCurlyBracket,
        "}" -> TokenData.CloseCurlyBracket,
        "let" -> TokenData.Let,
        "var" -> TokenData.Var,
        "=" -> TokenData.Equals,
        ":=" -> TokenData.Walrus,
        "||" -> TokenData.DoublePipe,
        "\\in" -> TokenData.SlashIn,
        "await" -> TokenData.Await,
        "def" -> TokenData.Def,
        "import" -> TokenData.Import,
        "module" -> TokenData.Module,
        "(" -> TokenData.OpenParenthesis,
        ")" -> TokenData.CloseParenthesis,
        "," -> TokenData.Comma
      )
        .sortWith(_._1 > _._1)
        .map{ case (keyword, tokenData) => withPosition{ str(keyword).map(_ => tokenData ) } }
        .reduce(_ | _)

    private val whitespace: Parser[Unit] =
      (
        elem(' ') | elem('\n') | elem('\t') | elem('\r') | elem('\f')
        )
        .map(_ => ())

    private val singleToken: Parser[Option[Token]] = (
      intLiteral | stringLiteral | fixedTokens | name
      ).map(Some(_)) |
      whitespace.map(_ => None)

    def apply(chars: IterableOnce[Char],
              startLine: Int = 1, startColumn: Int = 1,
              fileName: String): Iterator[Token] =
      Iterator.unfold(new CharsReader(
        fileName = fileName,
        elems = chars.iterator.to(LazyList),
        line = startLine,
        column = startColumn,
        lastPos = Position(fileName = fileName, line = startLine, column = startColumn),
      )) { reader =>
        if (reader.atEnd) {
          None
        } else {
          singleToken.apply(reader) match {
            case Success(tokenOpt, next) =>
              Some((tokenOpt, next.asInstanceOf[CharsReader]))
            case Failure(msg, _) => ??? // throw exception probably
            case Error(msg, _) => ??? // same
          }
        }
      }
        .flatten
  }
}
