package com.github.distcompiler.dcal

object DCalAST {
  /*
  module ::= `module` <name> <imports>? <definition>*

  imports ::= `import` <name> (`,` <name>)*

  definition ::=
    | `def` <name> `(` (<name> (`,` <name>)* )? `)` <block>

  block ::= `{` <statement>* `}

  statement ::=
    | `await` <expression>
    | <assign_pair> (`||` <assign_pair>)*
    | `let` <name> `=` <expression>
    | `var` <name> ((`=` | `\in`) <expression>)?
    | `if` <expression> `then` <block> (`else` <expression>)?

  // e.g `x := y || y := x` swaps x and y
  assign_pair ::= <name> `:=` <expression>

  expression ::= <expression_binop>

  expression_binop ::= <expression_base> (<binop> <expression_base>)?

  expression_base ::=
      | <boolean>
      | <int_literal>
      | <string_literal>
      | <name>
      | <set>
      | `(` <expression> `)`

  set ::= `{{` <expression>* `}}`

  boolean ::=
      | TRUE
      | FALSE

  binop ::= `+` | `-` | `=` | `#` | `<` | `>` | `<=` | `>=` | `\/` | `/\`
  */

  final case class Module(name: String, imports: List[String], definitions: List[Definition])
  final case class Definition(name: String, params: List[String], body: Block)
  final case class Block(statements: List[Statement])

  enum Statement {
    case Await(expression: Expression)
    case AssignPairs(assignPairs: List[AssignPair])
    case Let(name: String, expression: Expression)
    case Var(name: String, expressionOpt: Option[(BinOp, Expression)])
    case IfThenElse(predicate: Expression, thenBlock: Block, elseBlock: Block)
  }

  final case class AssignPair(name: String, expression: Expression)

  enum Expression {
    case ExpressionBinOp(lhs: Expression, binOp: DCalAST.BinOp, rhs: Expression)
    case True
    case False
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case Name(name: String)
    case Set(members: List[Expression])
    case BracketedExpression(expression: Expression)
  }

  enum BinOp {
    case EqualTo
    case SlashIn
    case Plus
    case Minus
    case NotEqualTo
    case LesserThan
    case GreaterThan
    case LesserThanOrEqualTo
    case GreaterThanOrEqualTo
    case And
    case Or
  }
}
