package forja.test

object CalcEval

// import forja.Lang

// trait Calc extends Lang:
//   object Num extends Lang.Term[(n: Int)]

//   object Expr extends Lang.Sum:
//     sealed trait Case extends Lang.Node
//     object Triv extends Lang.Term[(value: Num.T)], Case
//     object Add extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case
//     object Mul extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case
//   end Expr
// end Calc
// object Calc extends Calc

// trait Val extends Lang:
//   object Num extends Lang.Term[(n: Int)]
// end Val
// object Val extends Val

// object CalcEval:
//   given (
//       xform: => Lang.Transform[Calc.Expr.type, Val.Num.type],
//   ) => Lang.Rewrite[Calc.Expr.T, Val.Num.T]:
//     def rewrite(t: Calc.Expr.T): Val.Num.T =
//       t.ex match
//         case Calc.Expr.Add((l, r)) =>
//           val Val.Num(Tuple1(ln)) = xform.transform(l).runtimeChecked
//           val Val.Num(Tuple1(rn)) = xform.transform(r).runtimeChecked
//           Val.Num((n = ln + rn))
//         case Calc.Expr.Mul((l, r)) =>
//           val Val.Num(Tuple1(ln)) = xform.transform(l).runtimeChecked
//           val Val.Num(Tuple1(rn)) = xform.transform(r).runtimeChecked
//           Val.Num((n = ln * rn))
//         case Calc.Expr.Triv(Tuple1(Calc.Num(Tuple1(n)))) => Val.Num((n = n))

//   private val xform = summon[Lang.Transform[Calc.Expr.type, Val.Num.type]]

//   def eval(expr: Calc.Expr.T): Val.Num.T =
//     xform.transform(expr)

//   def main(args: Array[String]): Unit =
//     val e1 = Calc.Expr.Add(
//       lhs = Calc.Expr.Triv((value = Calc.Num((n = 2)))),
//       rhs = Calc.Expr.Triv((value = Calc.Num((n = 3)))),
//     )
//     println(s"2 + 3 = ${eval(e1)}")

//     val e2 = Calc.Expr.Mul(
//       lhs = Calc.Expr.Triv((value = Calc.Num((n = 4)))),
//       rhs = Calc.Expr.Triv((value = Calc.Num((n = 5)))),
//     )
//     println(s"4 * 5 = ${eval(e2)}")
//   end main
// end CalcEval
