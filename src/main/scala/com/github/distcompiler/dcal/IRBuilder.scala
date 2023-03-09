package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.Expression.{BracketedExpression, ExpressionBinOp, False, IntLiteral, Name, StringLiteral, True}
import com.github.distcompiler.dcal.DCalAST.Statement.{AssignPairs, Await, IfThenElse, Let, Var}
import com.github.distcompiler.dcal.DCalParser.*

import java.lang.Package
import scala.collection.mutable.ListBuffer

/**
 * Compiles DCal concrete syntax to an IR that resembles TLA+. DCal statements that compile to TLA+ identifiers, let
 * expressions, and set maps are preserved by their respective IR Node structures. All other DCal statements are
 * uninterpreted by this IR pass.
 *
 * Each DCal statement compiles to a TLA+ nested let expression, not another TLA+ statement in a one-to-one mapping
 * manner.
 */
object IRBuilder {
  enum NameInfo {
    case Local
    case State
  }

  final case class Context(stateName: String,
                           nameInfoOf: Map[String,NameInfo],
                           mapOnSetInfo: (String, String) = null) {

    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withMapOnSetInfo(setMember: String, set: String): Context =
      copy(mapOnSetInfo = (setMember, set))

    def withStateName(stateName: String): Context =
      copy(stateName = stateName)
  }

  private var lc = 1
  private var sc = 1

  def freshLocal: String = {
    val l = s"l$lc"
    lc = lc + 1
    l
  }

  def freshState: String = {
    val s = s"_state$sc"
    sc = sc + 1
    s
  }

  def generateBinOp(dcalBinOp: DCalAST.BinOp): IR.Node = {
    dcalBinOp match {
      case DCalAST.BinOp.Plus => IR.Node.Uninterpreted(" + ")
      case DCalAST.BinOp.Minus => IR.Node.Uninterpreted(" - ")
      case DCalAST.BinOp.LesserThanOrEqualTo => IR.Node.Uninterpreted(" <= ")
      case _ => ???
    }
  }

  def generateExpression(dcalExpr: DCalAST.Expression)(using ctx: Context): List[IR.Node] = {
    dcalExpr match {
      case ExpressionBinOp(lhs, binOp, rhs) =>
        generateExpression(lhs) ++ List(generateBinOp(binOp)) ++ generateExpression(rhs)

      case True => List(IR.Node.Uninterpreted("TRUE"))

      case False => List(IR.Node.Uninterpreted("FALSE"))

      case IntLiteral(value) => List(IR.Node.Uninterpreted(value.toString))

      case StringLiteral(value) => List(IR.Node.Uninterpreted(s""""$value""""))

      case Name(name) => ctx.nameInfoOf(name) match {
        case NameInfo.State => List(IR.Node.Name(ctx.mapOnSetInfo._1), IR.Node.Uninterpreted(s".$name"))
        case NameInfo.Local => List(IR.Node.Name(name))
      }

      case BracketedExpression(expr) =>
        List(IR.Node.Uninterpreted("(")) ++ generateExpression(expr) ++ List(IR.Node.Uninterpreted(")"))
    }
  }

  /**
   * Maps the DCal statement to the current state set, producing a new state set.
   * Examples:
   * str is a state, _state1 is the current state
   *    str := "new string" -> { [s EXCEPT !.str = "new string"]: s \in _state1 }
   * y & i are states, v is a local, _state1 is the current state
   *    y := y - v || i := i + 1 -> { [s EXCEPT !.y = s.y - v, !.i = s.i + 1 ]: s \in _state1 }
   */
  def generateAssignPairs(dcalAssignPairs: DCalAST.Statement.AssignPairs) (using ctx: Context): List[IR.Node] = {
    // 1: Creates a new local to use as set member
    // 2: Processes assign pairs
    //    For each assign pair <name> := <expression>
    //      - If <name> is a state, creates Uninterpreted(s"!.$name = "), else TODO
    //      - Calls generateExpression(expression): y - v -> s.y - v, i + i -> s.i + 1
    //      - Prepends Uninterpreted(s"!.$name = ") to generateExpression(expression) and returns
    // 3: Add a Uninterpreted(", ") between each assign pairs then inserts the result into s"[$setMember EXCEPT]
    // <processed assign pairs>]"

    /**
     * Examples:
     * str := "new string"  -> !.str = "new string"
     * y := y - v           -> !.y = s.y - v
     * i := i + 1           -> !.i = s.i + 1
     */
    // TODO: This assume that name in dcalAssignPair is always a state. The behaviour for a local name is undefined.
    def generateAssignPair(dcalAssignPair: DCalAST.AssignPair)(using ctx: Context): ListBuffer[IR.Node] =
      ctx.nameInfoOf(dcalAssignPair.name) match {
        case NameInfo.Local => ???
        case NameInfo.State =>
          // How to do the transformation y - v -> s.y - v? Adding "s" to the context mapping is not sufficient for y
          // to be transformed to "s.y".
          ListBuffer[IR.Node](IR.Node.Uninterpreted(s"!.${dcalAssignPair.name} = ")) ++= generateExpression(dcalAssignPair.expression)(using ctx).toBuffer
      }

    def generateDelimitedAssignPairs(aps: List[DCalAST.AssignPair])(using ctx: Context) = {
      def delimitHelper(lst: List[DCalAST.AssignPair], acc: ListBuffer[IR.Node]): ListBuffer[IR.Node] = {
        lst match {
          case Nil => acc
          case h::t => t match {
            case Nil => delimitHelper(t, acc :++ generateAssignPair(h)(using ctx))
            case _ => delimitHelper(t, acc :++ (generateAssignPair(h)(using ctx) += IR.Node.Uninterpreted(", ")))
          }
        }
      }
      delimitHelper(aps, ListBuffer[IR.Node]())
    }

    def generateProc(using ctx: Context): List[IR.Node] = {
      val pb = ListBuffer[IR.Node](
        IR.Node.Uninterpreted("["),
        IR.Node.Name(ctx.mapOnSetInfo._1),
        IR.Node.Uninterpreted(" EXCEPT "),
      )

      pb.appendAll(generateDelimitedAssignPairs(dcalAssignPairs.assignPairs)(using ctx))
      pb.append(IR.Node.Uninterpreted("]"))
      pb.toList
    }

    val setMember = freshLocal

    List(
      IR.Node.MapOnSet(
        set = List(IR.Node.Name(ctx.stateName)),
        setMember = setMember,
        proc = generateProc(using ctx.withMapOnSetInfo(setMember, ctx.stateName).withNameInfo(setMember, NameInfo.Local))
      )
    )
  }

  /**
   * Examples:
   * _state1 is the current state
   *    if x <= y then { x := x + 1 } else { y := y - 1 } ->
   *    UNION { IF s.x <= s.y
   *            THEN LET _state3 == { [s EXCEPT !.x = s.x + 1]: ss \in { s } } IN _state3
   *            ELSE LET _state3 == { [s EXCEPT !.y = s.y - 1]: ss \in { s } } IN _state3
   *          : s \in _state1 }
   */
  def generateIfThenElse(dcalIfThenElse: DCalAST.Statement.IfThenElse, rest: List[DCalAST.Statement])
                        (using ctx: Context): List[IR.Node]
  = {
    // 1: Returns a MapOnSet on the current state set, wrapped in UNION
    // 2: For the MapOnSet:
    //    - calls freshName to generate a local to use as the state set member
    //    - fills in IF ... THEN ... ELSE ... as the proc TODO: Define <pred> in AST
    def generateProc(using ctx: IRBuilder.Context): List[IR.Node] = {
      val pb = ListBuffer[IR.Node](IR.Node.Uninterpreted("IF "))
      val predicate = generateExpression(dcalIfThenElse.predicate)
      pb.appendAll(predicate)

      val thenState = freshState
      val thenBlock = IR.Node.Let(
        name = thenState,
        binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name(ctx.mapOnSetInfo._1), IR.Node.Uninterpreted(" }")),
        body = generateStatements(dcalIfThenElse.thenBlock.statements ++ rest)(using ctx.withStateName(thenState))
      )
      pb.append(IR.Node.Uninterpreted("THEN "))
      pb.append(thenBlock)

      val elseState = freshState
      val elseBlock = IR.Node.Let(
        name = elseState,
        binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name(ctx.mapOnSetInfo._1), IR.Node.Uninterpreted(" }")),
        body = generateStatements(dcalIfThenElse.elseBlock.statements ++ rest)(using ctx.withStateName(elseState))
      )
      pb.append(IR.Node.Uninterpreted("ELSE "))
      pb.append(elseBlock)

      pb.toList
    }

    val setMember = freshLocal
    List(
      IR.Node.Uninterpreted("UNION "),
      IR.Node.MapOnSet(
        set = List(IR.Node.Name(ctx.stateName)),
        setMember = setMember,
        proc = generateProc(using ctx.withMapOnSetInfo(setMember, ctx.stateName).withNameInfo(setMember, NameInfo
          .Local))
      )
    )
  }

  def generateLet(dcalLet: DCalAST.Statement.Let, rest: List[DCalAST.Statement])(using ctx: Context): List[IR.Node] = {
    // 1: Compiles a DCal let statement by generating a MapOnset whose proc is a LET expression. The LET expression's
    // name and binding should be the DCal let statement's name and expression respectively.
    // 2: Updates the ctx to contain the DCal name, the state name to be ???, then compiles the rest of the program.
    val setMember = freshLocal
    val newState = freshState
    List(
      IR.Node.Uninterpreted("UNION"),
      // { <proc>: <setMember> \in <stateName> }
      IR.Node.MapOnSet(
        set = List(IR.Node.Name(ctx.stateName)),
        setMember = setMember,
        // TODO: Add setMember to ctx of proc
        // LET <name> == <expr> IN LET <newstate> == { <setMember> } IN <rest>
        proc = List(
          IR.Node.Let(
            name = dcalLet.name,
            binding = generateExpression(dcalLet.expression),
            // <rest> where <name> exists, where <rest> is operated on { <setMember> }
            body = List(
              IR.Node.Let(
                name = newState,
                binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name(setMember), IR.Node.Uninterpreted(" }")),
                body = generateStatements(rest)(using ctx.withNameInfo(setMember, NameInfo.Local).withNameInfo
                (dcalLet.name, NameInfo.Local).withStateName(newState))
              )
            )
          )
        )
      )
    )
  }


  /**
   * Maps the DCal statement to the current state set, producing a new state set
   */
  def generateStatement(dcalStmt: DCalAST.Statement)
                       (using ctx: Context): List[IR.Node] = {
    dcalStmt match {
      case Await(expr) => List(IR.Node.Uninterpreted("await")) ++ generateExpression(expr)
      case Var(name: String, optExpression: Option[(DCalAST.BinOp, DCalAST.Expression)]) => ???
    }
  }

  /**
   * Recursively generates statements.
   * In the base case, when there is no DCal statement left to generate, produces the current state.
   * In the recursive case, when there is at least a DCal statement to generate, produces a LET expression whose:
   * - name is a new state,
   * - binding is a set of states produced by mapping the DCal statement to the current state,
   * - body is an expression produced by the generating the remaining DCal statements.
   *
   * Invariants: TODO
   */
  def generateStatements(dcalStmts: List[DCalAST.Statement])
                        (using ctx: Context): List[IR.Node] = {
    dcalStmts match {
      case Nil => List(IR.Node.Name(ctx.stateName))
      case s::ss =>
        s match {
          case assignPairs @ AssignPairs(_) => {
            val newCtx = ctx.withStateName(freshState)
            List(
              IR.Node.Let(
                name = newCtx.stateName,
                binding = generateAssignPairs(assignPairs),
                body = generateStatements(dcalStmts = ss)(using newCtx)
              )
            )
          }
          // LET <newstate> == <let + rest> IN <newstate>
          case let @ Let(_, _) => {
            val newState = freshState
            List(
              IR.Node.Let(
                name = newState,
                binding = generateLet(let, ss)(using ctx),
                body = List(IR.Node.Name(newState))
              )
            )
          }
          case ifThenElse @ IfThenElse(_, _, _) => {
            val newState = freshState
            List(
              IR.Node.Let(
                name = newState,
                binding = generateIfThenElse(ifThenElse, ss)(using ctx),
                body = List(IR.Node.Name(newState))
              )
            )
          }
          case _ => ???
        }
    }
  }

  def generateDefinition(dcalDef: DCalAST.Definition): IR.Definition = {
    lc = 1
    sc = 1
    var initialCtx = Context(
      nameInfoOf = Map[String, NameInfo](
        "str" -> NameInfo.State,
        "x" -> NameInfo.State,
        "y" -> NameInfo.State,
        "i" -> NameInfo.State
      ),
      stateName = freshState
    )

    dcalDef.params.foreach(
      param => initialCtx = initialCtx.withNameInfo(param, NameInfo.Local)
    )

    IR.Definition(
      name = dcalDef.name,
      params = initialCtx.stateName +: dcalDef.params,
      body = generateStatements(
        dcalStmts = dcalDef.body.statements
      )(using initialCtx)
    )
  }

  def generateDefinition(dcalImport: String): IR.Definition = {
    lc = 1
    sc = 1
    ???
  }

  def build(dcalModule: DCalAST.Module): IR.Module = {
    // Construct the IR Module to return, which holds all the generated TLA+ code
    val definitions = dcalModule.definitions.map(generateDefinition)
    val imports = dcalModule.imports.map(generateDefinition)
    IR.Module(
      name = dcalModule.name,
      definitions = imports ++ definitions,
    )
  }

  def apply(contents: String, fileName: String): IR.Module = {
    // Set up logging
    // Parse DCal contents
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
    // Build the IR
    build(dcalModule = dcalModule)
    // Log the IR
  }
}
