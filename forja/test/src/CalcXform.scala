package forja.test

object CalcXForm

// import forja.Lang

// trait SubLang extends Lang:
//   object Num extends Lang.Term[(n: Int)]

//   object Expr extends Lang.Sum:
//     sealed trait Case extends Lang.Node
//     object Triv extends Lang.Term[(value: Num.T)], Case
//     object Sub extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case
//     object Add extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case
//     object Mul extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case
//   end Expr
// end SubLang
// object SubLang extends SubLang

// trait AddLang extends Lang.Extend[SubLang]:
//   export up.{Expr as _, *}

//   object Expr extends up.Expr.Extends:
//     sealed trait Case extends Lang.Node
//     export up.Expr.{Triv, Add, Mul}
//   end Expr

//   given up.Expr.Sub.Retract
// end AddLang
// object AddLang extends AddLang

// object CalcXform:
//   given (
//       xform: => Lang.Transform[SubLang.Expr.type, AddLang.Expr.type],
//   ) => Lang.Rewrite[SubLang.Expr.Sub.T, AddLang.Expr.T]:
//     def rewrite(t: SubLang.Expr.Sub.T): AddLang.Expr.T =
//       val SubLang.Expr.Sub((l, r)) = t.runtimeChecked
//       AddLang.Expr.Add(
//         lhs = ???, // xform.transform(l),
//         rhs = AddLang.Expr.Mul(
//           lhs = AddLang.Expr.Triv((value = AddLang.Num((n = -1)))),
//           rhs = ???, // xform.transform(r),
//         ),
//       )

//   private val xform =
//     summon[Lang.Transform[SubLang.Expr.type, AddLang.Expr.type]]

//   def transform(expr: SubLang.Expr.T): AddLang.Expr.T =
//     xform.transform(expr)

//   def main(args: Array[String]): Unit =
//     val e1 = SubLang.Expr.Sub(
//       lhs = SubLang.Expr.Triv((value = SubLang.Num((n = 2)))),
//       rhs = SubLang.Expr.Triv((value = SubLang.Num((n = 3)))),
//     )
//     println(e1)
//     println(transform(e1))
//   end main
// end CalcXform
