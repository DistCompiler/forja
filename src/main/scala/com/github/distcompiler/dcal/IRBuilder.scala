package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{AssignmentOp, Statement}
import com.github.distcompiler.dcal.DCalAST.Expression.{BracketedExpression, ExpressionBinOp, ExpressionLogicOp, ExpressionRelOp, False, IntLiteral, Name, Set, StringLiteral, True}
import com.github.distcompiler.dcal.DCalAST.Statement.{AssignPairs, Await, IfThenElse, Let, Var}
import com.github.distcompiler.dcal.DCalParser.*

import scala.annotation.tailrec
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

  inline def ctx(using ctx: Context): Context = ctx

  final case class Context(stateName: String,
                           nameInfoOf: Map[String,NameInfo],
                           mapFilterOnSetInfo: (String, List[IR.Node]) = null,
                           rootsAndCounters: Map[String,Int] = Map.empty) {

    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withMapFilterOnSetInfo(setMember: String, set: List[IR.Node]): Context =
      copy(mapFilterOnSetInfo = (setMember, set))

    def withStateName(stateName: String): Context =
      copy(stateName = stateName)

    def withFreshStateName[T](fn: Context ?=> String => T): T =
      withFreshName("_state") { stateName =>
        fn(using ctx.withStateName(stateName))(stateName)
      }

    def withFreshName[T](hint: String = "_name")(fn: Context ?=> String => T): T = {
      val (cleanName, newRootsAndCounters) = rootsAndCounters.get(hint) match {
        case None =>
          (s"$hint${1}", rootsAndCounters.updated(hint, 1))
        case Some(count) =>
          (s"$hint$count", rootsAndCounters.updated(hint, count + 1))
      }
      given Context = copy(rootsAndCounters = newRootsAndCounters)
      fn(cleanName)
    }
  }

  private var lc = 1
  private var ac = 1

  private def freshLocal: String =
    val l = s"l$lc"
    lc = lc + 1
    l

  private def freshAnon: String =
    val a = s"_anon$ac"
    ac = ac + 1
    a

  def generateBinOp(dcalBinOp: DCalAST.BinOp): IR.Node = {
    dcalBinOp match {
      case DCalAST.BinOp.Plus => IR.Node.Uninterpreted(" + ")
      case DCalAST.BinOp.Minus => IR.Node.Uninterpreted(" - ")
    }
  }

  def generateRelOp(dcalRelOp: DCalAST.RelOp): IR.Node = {
    dcalRelOp match {
      case DCalAST.RelOp.LesserThan => IR.Node.Uninterpreted(" < ")
      case DCalAST.RelOp.LesserThanOrEqualTo => IR.Node.Uninterpreted(" <= ")
      case DCalAST.RelOp.GreaterThan => IR.Node.Uninterpreted(" > ")
      case DCalAST.RelOp.GreaterThanOrEqualTo => IR.Node.Uninterpreted(" >= ")
      case DCalAST.RelOp.EqualTo => IR.Node.Uninterpreted(" = ")
      case DCalAST.RelOp.NotEqualTo => IR.Node.Uninterpreted(" # ")
    }
  }

  def generateExpression(dcalExpr: DCalAST.Expression)(using Context): List[IR.Node] = {
    dcalExpr match {
      case ExpressionBinOp(lhs, binOp, rhs) =>
        generateExpression(lhs) ++ List(generateBinOp(binOp)) ++ generateExpression(rhs)

      case ExpressionRelOp(lhs, relOp, rhs) =>
        generateExpression(lhs) ++ List(generateRelOp(relOp)) ++ generateExpression(rhs)

      case True => List(IR.Node.Uninterpreted("TRUE"))

      case False => List(IR.Node.Uninterpreted("FALSE"))

      case IntLiteral(value) => List(IR.Node.Uninterpreted(value.toString))

      case StringLiteral(value) => List(IR.Node.Uninterpreted(s""""$value""""))

      case Name(name) => ctx.nameInfoOf(name) match {
        case NameInfo.State => List(IR.Node.Name(ctx.mapFilterOnSetInfo._1), IR.Node.Uninterpreted(s".$name"))
        case NameInfo.Local => List(IR.Node.Name(name))
      }

      case BracketedExpression(expr) =>
        List(IR.Node.Uninterpreted("(")) ++ generateExpression(expr) ++ List(IR.Node.Uninterpreted(")"))

      case ExpressionLogicOp(_, _, _) => ???

      case Set(members) =>
        @tailrec
        def delimit(lst: List[DCalAST.Expression], acc: ListBuffer[IR.Node]): ListBuffer[IR.Node] = {
          lst match {
            case Nil => acc
            case h :: t => t match {
              case Nil => delimit(t, acc :++ generateExpression(h)(using ctx))
              case _ => delimit(t, acc :++ (generateExpression(h)(using ctx) :+ IR.Node.Uninterpreted(", ")))
            }
          }
        }

        (delimit(members, ListBuffer[IR.Node](IR.Node.Uninterpreted("{ "))) += IR.Node.Uninterpreted(" }")).toList
    }
  }

  def generateAwait(dcalAwait: DCalAST.Statement.Await)(using Context): List[IR.Node] = {
    val setMember = freshLocal
    List(
      IR.Node.FilterOnSet(
        set = List(IR.Node.Name(ctx.stateName)),
        setMember = setMember,
        pred = generateExpression(dcalAwait.expression)(using ctx.withNameInfo(setMember, NameInfo.Local)
          .withMapFilterOnSetInfo(setMember, List(IR.Node.Name(ctx.stateName))))
      )
    )
  }

  /**
   * Maps the DCal statement to the current state set, producing a new state set.
   * Examples:
   * Assume str is a state, _state1 is the current state
   *    str := "new string" -> { [s EXCEPT !.str = "new string"]: s \in _state1 }
   * Assume y & i are states, v is a local, _state1 is the current state
   *    y := y - v || i := i + 1 -> { [s EXCEPT !.y = s.y - v, !.i = s.i + 1 ]: s \in _state1 }
   */
  def generateAssignPairs(dcalAssignPairs: DCalAST.Statement.AssignPairs)(using Context): List[IR.Node] = {
    /**
     * Examples: Assume str, y, i are all state variables
     * str := "new string"  -> !.str = "new string"
     * y := y - v           -> !.y = s.y - v
     * i := i + 1           -> !.i = s.i + 1
     */
    // TODO: This assume that name in dcalAssignPair is always a state. The behaviour for a local name is undefined.
    def generateAssignPair(dcalAssignPair: DCalAST.AssignPair)(using Context): ListBuffer[IR.Node] =
      ctx.nameInfoOf(dcalAssignPair.name) match {
        case NameInfo.Local => ???
        case NameInfo.State =>
          ListBuffer[IR.Node](IR.Node.Uninterpreted(s"!.${dcalAssignPair.name} = ")) ++= generateExpression(dcalAssignPair.expression)(using ctx).toBuffer
      }

    def generateDelimitedAssignPairs(aps: List[DCalAST.AssignPair])(using Context) = {
      @tailrec
      def delimit(lst: List[DCalAST.AssignPair], acc: ListBuffer[IR.Node]): ListBuffer[IR.Node] = {
        lst match {
          case Nil => acc
          case h::t => t match {
            case Nil => delimit(t, acc :++ generateAssignPair(h)(using ctx))
            case _ => delimit(t, acc :++ (generateAssignPair(h)(using ctx) += IR.Node.Uninterpreted(", ")))
          }
        }
      }
      delimit(aps, ListBuffer[IR.Node]())
    }

    def generateProc(using Context): List[IR.Node] = {
      val pb = ListBuffer[IR.Node](
        IR.Node.Uninterpreted("["),
        IR.Node.Name(ctx.mapFilterOnSetInfo._1),
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
        proc = generateProc(using ctx
          .withNameInfo(setMember, NameInfo.Local)
          .withMapFilterOnSetInfo(setMember, List(IR.Node.Name(ctx.stateName))))
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
  def generateIfThenElse(dcalIfThenElse: DCalAST.Statement.IfThenElse)
                        (using Context): List[IR.Node]
  = {
    // TODO: Define <pred> in AST, if <pred> then <block> else <block> | if <pred> then <pred> else <pred>
    def generateProc(using ctx: IRBuilder.Context): List[IR.Node] = {
      val pb = ListBuffer[IR.Node](IR.Node.Uninterpreted("IF "))
      val predicate = generateExpression(dcalIfThenElse.predicate)
      pb.appendAll(predicate)

      ctx.withFreshStateName( newStateName =>
        val thenBlock = IR.Node.Let(
          name = newStateName,
          binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name(ctx.mapFilterOnSetInfo._1), IR.Node.Uninterpreted(" }")),
          body = generateStatements(dcalIfThenElse.thenBlock.statements)
        )
        pb.append(IR.Node.Uninterpreted("\nTHEN "))
        pb.append(thenBlock)
      )

      ctx.withFreshStateName( newStateName =>
        val elseBlock = IR.Node.Let(
          name = newStateName,
          binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name(ctx.mapFilterOnSetInfo._1), IR.Node.Uninterpreted(" }")),
          body = generateStatements(dcalIfThenElse.elseBlock.statements)
        )
        pb.append(IR.Node.Uninterpreted("\nELSE "))
        pb.append(elseBlock)
      )

      pb.toList
    }

    val setMember = freshLocal
    List(
      IR.Node.Uninterpreted("UNION "),
      IR.Node.MapOnSet(
        set = List(IR.Node.Name(ctx.stateName)),
        setMember = setMember,
        proc = generateProc(using ctx
          .withMapFilterOnSetInfo(setMember, List(IR.Node.Name(ctx.stateName)))
          .withNameInfo(setMember, NameInfo
            .Local))
      )
    )
  }

  def generateLet(dcalLet: DCalAST.Statement.Let, rest: List[DCalAST.Statement])(using Context): List[IR.Node] = {
    dcalLet.assignmentOp match
      case AssignmentOp.EqualTo =>
        val setMember = freshLocal
        List(
          IR.Node.Uninterpreted("UNION"),
          IR.Node.MapOnSet(
            set = List(IR.Node.Name(ctx.stateName)),
            setMember = setMember,
            // TODO: Add setMember to ctx of proc
            proc = ctx
              .withMapFilterOnSetInfo(setMember, List(IR.Node.Name(ctx.stateName)))
              .withFreshStateName( newStateName =>
                List(
                  IR.Node.Let(
                    name = dcalLet.name,
                    binding = generateExpression(dcalLet.expression),
                    body = List(
                      IR.Node.Let(
                        name = newStateName,
                        binding = List(
                          IR.Node.Uninterpreted("{ "),
                          IR.Node.Name(setMember),
                          IR.Node.Uninterpreted(" }")
                        ),
                        body = generateStatements(rest)(using ctx
                          .withNameInfo(setMember, NameInfo.Local)
                          .withNameInfo(dcalLet.name, NameInfo.Local))
                      )
                    )
                  )
              )
            )
          )
        )
      case AssignmentOp.SlashIn =>
        val outerSetMember = freshLocal

        def generateInnerProc(using Context): List[IR.Node] =
          ctx.withFreshStateName( newStateName =>
            List(
              IR.Node.Let(
                name = newStateName,
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name(outerSetMember),
                  IR.Node.Uninterpreted(" }")
                ),
                body = generateStatements(rest)
              )
            )
          )

        def generateOuterProc(using Context): List[IR.Node] =
          val innerSetMember = dcalLet.name
          val set = generateExpression(dcalLet.expression)
          List(
            IR.Node.Uninterpreted("UNION "),
            IR.Node.MapOnSet(
              set = set,
              setMember = innerSetMember,
              proc = generateInnerProc(using ctx
                .withNameInfo(innerSetMember, NameInfo.Local)
              )
            )
          )

        List(
          IR.Node.Uninterpreted("UNION "),
          IR.Node.MapOnSet(
            set = List(IR.Node.Name(ctx.stateName)),
            setMember = outerSetMember,
            proc = generateOuterProc(using ctx
              .withNameInfo(outerSetMember, NameInfo.Local)
              .withMapFilterOnSetInfo(outerSetMember, List(IR.Node.Name(ctx.stateName)))
            )
          )
        )
  }

  def generateVar(dcalVar: DCalAST.Statement.Var)(using Context): List[IR.Node] = {
    val name = dcalVar.name
    val assignmentOp = dcalVar.expressionOpt.get._1
    val expr = dcalVar.expressionOpt.get._2
    assignmentOp match {
      case DCalAST.AssignmentOp.EqualTo =>
        val setMember = freshLocal
        val setKey = freshLocal
        List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name(ctx.stateName)),
            setMember = setMember,
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Uninterpreted(s"""$setKey \\in DOMAIN """),
              IR.Node.Name(setMember),
              IR.Node.Uninterpreted(s""" \\cup { "$name" } |-> IF $setKey = "$name" THEN """),
              // TODO: Throw error if expressionOpt matches None
            ) ++
              generateExpression(expr) ++
              List(
                IR.Node.Uninterpreted(" ELSE "),
                IR.Node.Name(setMember),
                IR.Node.Uninterpreted(s"[$setKey]]"),
              )
          )
        )
      case DCalAST.AssignmentOp.SlashIn => ??? // Should be unreachable
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
                        (using Context): List[IR.Node] = {
    dcalStmts match {
      case Nil => List(IR.Node.Name(ctx.stateName))
      case s::ss =>
        var sInTla: List[IR.Node] = Nil
        s match {
          case await @ Await(_) =>
            sInTla = generateAwait(await)
          case assignPairs @ AssignPairs(_) =>
            sInTla = generateAssignPairs(assignPairs)
          case let @ Let(_, _, _) =>
            sInTla = generateLet(let, ss)
          case `var` @ Var(_, _) =>
            sInTla = generateVar(`var`)
          case ifThenElse @ IfThenElse(_, _, _) =>
            sInTla = generateIfThenElse(ifThenElse)
        }
        s match
          case Let(_, _, _) => sInTla
          case Var(name, _) =>
            ctx.withFreshStateName(newStateName =>
              List(
                IR.Node.Let(
                  name = newStateName,
                  binding = sInTla,
                  body = generateStatements(ss)(using ctx.withNameInfo(name, NameInfo.State))
                )
              )
            )
          case _ =>
            ctx.withFreshStateName(newStateName =>
              List(
                IR.Node.Let(
                  name = newStateName,
                  binding = sInTla,
                  body = generateStatements(dcalStmts = ss)
                )
              )
            )
    }
  }


  /**
   * Transforms all "var <name> \in <expression>" into the sequence
   * "let <anon-name> \in <expression>" followed by "var <name> = <anon-name>"
   */
  def preprocessStatements(dcalStmts: List[DCalAST.Statement]): List[DCalAST.Statement] =
    val transformed = ListBuffer[DCalAST.Statement]()
    dcalStmts.foreach { dcalStmt =>
      dcalStmt match
        case Var(name, Some(DCalAST.AssignmentOp.SlashIn, expr)) =>
          val newAnon = freshAnon
          transformed.append(
            DCalAST.Statement.Let(
              name = newAnon,
              assignmentOp = DCalAST.AssignmentOp.SlashIn,
              expression = expr
            )
          )
          transformed.append(
            DCalAST.Statement.Var(
              name = name,
              expressionOpt = Some(DCalAST.AssignmentOp.EqualTo, DCalAST.Expression.Name(newAnon))
            )
          )
        case _ => transformed.append(dcalStmt)
    }
    transformed.toList

  def generateDefinition(dcalDef: DCalAST.Definition): IR.Definition = {
    lc = 1
    ac = 1
    var initialCtx = Context(
      nameInfoOf = Map[String, NameInfo](
        "str" -> NameInfo.State,
        "x" -> NameInfo.State,
        "y" -> NameInfo.State,
        "i" -> NameInfo.State,
        "set" -> NameInfo.State
      ),
      stateName = "_state1",
      rootsAndCounters = Map[String, Int]("_state" -> 2)
    )

    dcalDef.params.foreach(
      param => initialCtx = initialCtx.withNameInfo(param, NameInfo.Local)
    )

    IR.Definition(
      name = dcalDef.name,
      params = initialCtx.stateName +: dcalDef.params,
      body = generateStatements(
        dcalStmts = preprocessStatements(dcalDef.body.statements)
      )(using initialCtx)
    )
  }

  def generateDefinition(dcalImport: String): IR.Definition = {
    lc = 1
    ac = 1
    ???
  }

  def build(dcalModule: DCalAST.Module): IR.Module = {
    val definitions = dcalModule.definitions.map(generateDefinition)
    val imports = dcalModule.imports.map(generateDefinition)
    IR.Module(
      name = dcalModule.name,
      definitions = imports ++ definitions,
    )
  }

  def apply(contents: String, fileName: String): IR.Module = {
    // TODO: Set up logging
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
    build(dcalModule = dcalModule)
  }
}
