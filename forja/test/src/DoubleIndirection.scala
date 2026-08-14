package forja.test

object DoubleIndirection

// import forja.Lang

// trait DL1 extends Lang:
//   object Expr extends Lang.Sum:
//     sealed trait Case extends Lang.Node
//     object Foo extends Lang.Term[(a: Expr.T)], Case
//     object Bar extends Lang.Term[(b: Expr.T)], Case
//     object Leaf extends Lang.Term[(n: Int)], Case
//   end Expr
// end DL1
// object DL1 extends DL1

// trait DL2 extends Lang.Extend[DL1]:
//   export up.{Expr as _, *}

//   object Expr extends up.Expr.Extends:
//     sealed trait Case extends Lang.Node
//     export up.Expr.{Bar, Leaf}
//   end Expr

//   given up.Expr.Foo.Retract
// end DL2
// object DL2 extends DL2

// trait DL3 extends Lang.Extend[DL2]:
//   export up.{Expr as _, *}

//   object Expr extends up.Expr.Extends:
//     sealed trait Case extends Lang.Node
//     export up.Expr.Leaf
//   end Expr

//   given up.Expr.Bar.Retract
// end DL3
// object DL3 extends DL3

// object DoubleIndirection:
//   given removeFoo: (
//       xform: => Lang.Transform[DL1.Expr.type, DL2.Expr.type],
//   ) => Lang.Rewrite[DL1.Expr.Foo.T, DL2.Expr.T]:
//     def rewrite(t: DL1.Expr.Foo.T): DL2.Expr.T =
//       val DL1.Expr.Foo(Tuple1(a)) = t.runtimeChecked
//       xform.transform(a)

//   given removeBar: (
//       xform: => Lang.Transform[DL2.Expr.type, DL3.Expr.type],
//   ) => Lang.Rewrite[DL2.Expr.Bar.T, DL3.Expr.T]:
//     def rewrite(t: DL2.Expr.Bar.T): DL3.Expr.T =
//       val DL2.Expr.Bar(Tuple1(b)) = t.runtimeChecked
//       xform.transform(b.asInstanceOf[DL2.Expr.T]) // need a cast here :(

//   private val xform1 = summon[Lang.Transform[DL1.Expr.type, DL2.Expr.type]]
//   private val xform2 = summon[
//     Lang.Transform[DL2.Expr.type, DL3.Expr.type],
//   ] // why does LSP complain but seems to work fine?

//   def main(args: Array[String]): Unit =
//     val e = DL1.Expr.Foo(
//       (
//         a = DL1.Expr.Bar(
//           (
//             b = DL1.Expr.Leaf((n = 42)),
//           ),
//         ),
//       ),
//     )
//     println(s"DL1: $e")
//     val e2 = xform1.transform(e)
//     println(s"DL2: $e2")
//     val e3 = xform2.transform(e2)
//     println(s"DL3: $e3")
//   end main
// end DoubleIndirection
