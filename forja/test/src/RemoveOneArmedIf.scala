package forja.test

object RemoveOneArmedIf

// import forja.Lang

// trait If1Lang extends Lang:
//   object Triv extends Lang.Sum:
//     sealed trait Case extends Lang.Node
//     object Num extends Lang.Term[(n: Int)], Case
//   end Triv

//   object Pred extends Lang.Sum:
//     sealed trait Case extends Lang.Node
//     object IsZero extends Lang.Term[(exp: Expr.T)], Case
//   end Pred

//   object Expr extends Lang.Sum:
//     sealed trait Case extends Lang.Node

//     object If extends Lang.Term[(pred: Pred.T, con: Expr.T, alt: Expr.T)], Case
//     object If1 extends Lang.Term[(pred: Pred.T, con: Expr.T)], Case

//     object Add extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case
//     object Sub extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case
//     object Mul extends Lang.Term[(lhs: Expr.T, rhs: Expr.T)], Case

//     object Triv extends Lang.Term[(value: If1Lang.Triv.T)], Case
//   end Expr
// end If1Lang
// object If1Lang extends If1Lang

// trait IfLang extends Lang.Extend[If1Lang]:
//   export up.{Expr as _, Triv as _, *}

//   object Triv extends up.Triv.Extends:
//     sealed trait Case extends Lang.Node
//     export up.Triv.Num
//     object Void extends Lang.Term[(unit: Unit)], Case
//   end Triv

//   object Expr extends up.Expr.Extends:
//     sealed trait Case extends Lang.Node
//     export up.Expr.{If, Add, Sub, Mul, Triv as _}
//     object Triv extends Lang.Term[(value: IfLang.Triv.T)], Case
//   end Expr

//   given up.Expr.If1.Retract
// end IfLang
// object IfLang extends IfLang

// object RemoveOneArmedIf:
//   private val voidLit: IfLang.Expr.T =
//     IfLang.Expr.Triv((value = IfLang.Triv.Void((unit = ()))))

//   given (xformExpr: => Lang.Transform[If1Lang.Expr.type, IfLang.Expr.type])
//     => (
//         xformPred: => Lang.Transform[If1Lang.Pred.type, IfLang.Pred.type],
//   ) => Lang.Rewrite[If1Lang.Expr.If1.T, IfLang.Expr.T]:
//     def rewrite(t: If1Lang.Expr.If1.T): IfLang.Expr.T =
//       val If1Lang.Expr.If1((pred, con)) = t.runtimeChecked
//       IfLang.Expr.If(
//         pred = xformPred.transform(pred),
//         con = ???, // xformExpr.transform(con),
//         alt = ???, // voidLit,
//       )

//   private val exprTransform =
//     summon[Lang.Transform[If1Lang.Expr.type, IfLang.Expr.type]]

//   def transform(expr: If1Lang.Expr.T): IfLang.Expr.T =
//     exprTransform.transform(expr)

//   def main(args: Array[String]): Unit =
//     val num = If1Lang.Triv.Num((n = 42))
//     val lit: If1Lang.Expr.T = If1Lang.Expr.Triv((value = num))
//     val e = If1Lang.Expr.If1(
//       pred = If1Lang.Pred.IsZero((exp = lit)),
//       con = If1Lang.Expr.Add(lhs = lit, rhs = lit),
//     )
//     println(s"Before: $e")
//     println(s"After:  ${transform(e)}")
//   end main
// end RemoveOneArmedIf
