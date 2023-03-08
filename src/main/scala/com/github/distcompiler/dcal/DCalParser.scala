package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.*
import com.github.distcompiler.dcal.DCalTokenizer.*

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object DCalParser {
  object parse extends Parsers {
    override type Elem = DCalTokenData

    class TokensReader(elems: LazyList[Token]) extends Reader[DCalTokenData] {
      override def first: DCalTokenData = elems.head.data

      override def atEnd: Boolean = elems.isEmpty

      override def pos: Position = NoPosition

      override def rest: Reader[DCalTokenData] = new TokensReader(elems.tail)
    }

    def apply(tokens: Iterator[Token]): DCalAST.Module = {
      val reader = new TokensReader(tokens.to(LazyList))
      module(reader) match {
        case Success(mdl, _) => mdl
        case Failure(msg, _) => ???
        case Error(msg, _) => ???
      }
    }

    lazy val name: Parser[String] = {
      acceptMatch("name", { case DCalTokenData.Name(name) => name })
    }

    /**
     *
     * @param elemParser      a parser that matches an element of type T
     * @param delimiterParser a parser that matches an element of type D
     * @tparam T the type of elements if successfully parsed
     * @tparam D the type of delimiter elements
     * @return the parser that matches a delimited sequence of elements of type T
     */
    private def delimited[T, D](elemParser: => Parser[T], delimiterParser: Parser[D] = DCalTokenData.Comma): Parser[List[T]] = {
      (elemParser ~ rep(delimiterParser ~> elemParser)).map {
        case t ~ ts => t :: ts
      }
    }

    lazy val module: Parser[DCalAST.Module] = {
      (elem(DCalTokenData.Module) ~> name ~ opt(imports) ~ rep(definition)).map {
        case name ~ importsOpt ~ definitions =>
          DCalAST.Module(
            name = name,
            imports = importsOpt.getOrElse(Nil),
            definitions = definitions,
          )
      }
    }

    lazy val imports: Parser[List[String]] = {
      (elem(DCalTokenData.Import) ~> delimited(name)).map {
        case imports => imports
      }
    }

    lazy val definition: Parser[DCalAST.Definition] = {
      (elem(DCalTokenData.Def) ~> name ~ elem(DCalTokenData.OpenParenthesis) ~ opt(delimited(name)) ~ elem(DCalTokenData.CloseParenthesis) ~ block).map {
        case name ~ _ ~ params ~ _ ~ block =>
          DCalAST.Definition(
            name = name, params = params.getOrElse(Nil), body = block
          )
      }
    }

    lazy val block: Parser[DCalAST.Block] = {
      (elem(DCalTokenData.OpenCurlyBracket) ~> rep(statement) <~ elem(DCalTokenData.CloseCurlyBracket))
        .map { statements =>
          DCalAST.Block(statements)
        }
    }

    lazy val assignPair: Parser[DCalAST.AssignPair] = (name ~ elem(DCalTokenData.Walrus) ~ expression).map {
      case name ~ _ ~ expr => DCalAST.AssignPair(name = name, expression = expr)
    }

    lazy val statement: Parser[DCalAST.Statement] = {
      val await = (elem(DCalTokenData.Await) ~> expression).map(expr => DCalAST.Statement.Await(expr))

      val ifThenElse = (elem(DCalTokenData.If) ~> expression ~ elem(DCalTokenData.Then) ~ block ~ elem(DCalTokenData.Else) ~ block).map {
        case predicate ~ _ ~ thenBlock ~ _ ~ elseBlock => DCalAST.Statement.IfThenElse(
          predicate = predicate, thenBlock = thenBlock, elseBlock = elseBlock
        )
      }

      val assignPairs =
        delimited(elemParser = assignPair, delimiterParser = elem(DCalTokenData.DoublePipe)).map {
          pairs => DCalAST.Statement.AssignPairs(pairs)
        }

      val let = (elem(DCalTokenData.Let) ~> name ~ elem(DCalTokenData.EqualTo) ~ expression).map {
        case name ~ _ ~ expr => DCalAST.Statement.Let(name = name, expression = expr)
      }

      val op = acceptMatch("var operator", {
        case DCalTokenData.EqualTo => DCalAST.BinOp.EqualTo
        case DCalTokenData.SlashIn => DCalAST.BinOp.SlashIn
      })

      val `var` =
        (elem(DCalTokenData.Var) ~> name ~ opt(op ~ expression)).map {
          case name ~ Some(opOpt ~ exprOpt) => DCalAST.Statement.Var(name = name, expressionOpt = Some((opOpt, exprOpt)))
          case name ~ None => DCalAST.Statement.Var(name = name, expressionOpt = None)
        }

      await | ifThenElse | let | `var` | assignPairs
    }

    // TODO: Operator precedence behaviour needs reworking
    lazy val expression: Parser[DCalAST.Expression] = expressionBinOp

    lazy val expressionBinOp: Parser[DCalAST.Expression] = {
      (expressionBase ~ opt(binOp ~ expressionBase)).map {
        case lhs ~ rhsOpt =>
          rhsOpt
            .map {
              case binOp ~ rhs => DCalAST.Expression.ExpressionBinOp(
                lhs = lhs, binOp = binOp, rhs = rhs
              )
            }
            .getOrElse(lhs)
      }
    }

    lazy val expressionBase: Parser[DCalAST.Expression] = {
      val literal = acceptMatch("literal", {
        case DCalTokenData.IntLiteral(v) => DCalAST.Expression.IntLiteral(v)
        case DCalTokenData.StringLiteral(v) => DCalAST.Expression.StringLiteral(v)
      })

      val nameExpr = name.map(str => DCalAST.Expression.Name(str))

      val bracketedExpr = (elem(DCalTokenData.OpenParenthesis) ~> expression <~ elem(DCalTokenData.CloseParenthesis)).map { expr => expr }

      val boolean = acceptMatch("boolean", {
        case DCalTokenData.True => DCalAST.Expression.True
        case DCalTokenData.False => DCalAST.Expression.False
      })

      val set = (elem(DCalTokenData.OpenCurlyBracket) ~> elem(DCalTokenData.OpenCurlyBracket) ~> opt(delimited(expression)) <~
        elem(DCalTokenData.CloseCurlyBracket) <~ elem(DCalTokenData.CloseCurlyBracket)).map { setMembers =>
        DCalAST.Expression.Set(members = setMembers.getOrElse(Nil))
      }

      bracketedExpr | set | boolean | literal | nameExpr
    }

    lazy val binOp: Parser[DCalAST.BinOp] =
      acceptMatch("binOp", {
        case DCalTokenData.Plus => DCalAST.BinOp.Plus
        case DCalTokenData.Minus => DCalAST.BinOp.Minus
        case DCalTokenData.EqualTo => DCalAST.BinOp.EqualTo
        case DCalTokenData.NotEqualTo => DCalAST.BinOp.NotEqualTo
        case DCalTokenData.LesserThan => DCalAST.BinOp.LesserThan
        case DCalTokenData.LesserThanOrEqualTo => DCalAST.BinOp.LesserThanOrEqualTo
        case DCalTokenData.GreaterThan => DCalAST.BinOp.GreaterThan
        case DCalTokenData.GreaterThanOrEqualTo => DCalAST.BinOp.GreaterThanOrEqualTo
        case DCalTokenData.Or => DCalAST.BinOp.Or
        case DCalTokenData.And => DCalAST.BinOp.And
      })
  }

  def apply(contents: String, fileName: String): DCalAST.Module =
    val tokens = tokenize(chars = contents, fileName = fileName)
    parse(tokens)
}
