package forja.test

object Test:
//   def main(args: Array[String]): Unit =
//     erased val a = summon[Lang.EffectiveType[(Int, Int)]]
//     erased val b = Lang.effectiveNamedTuple[(x: Int, y: Int)]
//     erased val c: Lang.EffectiveType.Aux[(x: String, y: Int), (x: String, y: Int)] = summon[Lang.EffectiveType[(x: String, y: Int)]]


//     trait L1 extends Lang:
//       object Foo extends Lang.Term[(i: Int, j: Int)]

//       object Ping extends Lang.Sum:
//         sealed trait Case extends Lang.Node

//         object Pong extends Lang.Term[(k: Int, foo: Foo.T)], Case
//         object Bob extends Lang.Term[(k: Int, foo: Foo.T)], Case
//       end Ping
//     end L1
//     object L1 extends L1

//     val x = L1.Foo(i = 42, j = 43)
//     println(x)
//     //val ping: L1.Ping.T = L1.Ping.Pong(k = 12, foo = x)
//     // println(ping)

//     trait L2 extends Lang.Extend[L1]:
//       export up.{Foo as _, *}
//       object Bar extends Lang.Term[(s: String, opt: Option[up.Foo.T])]

//       given r1: up.Foo.ReplaceWith[Bar.type]()
//       given r2: up.Ping.Pong.ReplaceWith[Bar.type]()
//       // given up.Foo.Retract
//       // given up.Ping.Bob.Retract()
//     end L2
//     object L2 extends L2

//     val y: L2.Bar.T = L2.Bar(s = "hi", opt = Some(L2.Bar("ho", None)))
//     println(y)
//     // val ping2: L2.Ping.T = y
//     // println(ping2)

//     // y match
//     //   case L2.Bar(s, opt) =>
//     //     println((s, opt))
//     // end match

//     // ping2.ex match
//     //   case L2.Bar((s, opt)) =>
//     //     println(s"$s, $opt")
//     //   // case L2.Ping.Bob(s, opt) =>
//     //   //   println("bob")
//     // end match

//     innerSumTest()
//   end main

//   def innerSumTest(): Unit =
//     trait L1 extends Lang:
//       object Foo extends Lang.Term[(i: Int, j: Int)]

//       object Ping extends Lang.Sum:
//         sealed trait Case extends Lang.Node
//         object Case:
//           object Pong extends Lang.Term[(k: Int, foo: Foo.T)], Case
//           object Bob extends Lang.Term[(k: Int, foo: Foo.T)], Case
//         export Case.*
//       end Ping
//     end L1
//     object L1 extends L1

//     trait L2 extends Lang.Extend[L1]:
//       export up.{Foo as _, Ping as _, *}
//       object Bar extends Lang.Term[(s: String, opt: Option[up.Foo.T])]
//       object Ping extends up.Ping.Extends:
//         sealed trait Case extends Lang.Node
//         object Case:
//           export up.Ping.Case.*
//           object NewCase
//               extends Lang.Term[(s: String, opt: Option[up.Foo.T])],
//                 Case
//         export Case.*
//       end Ping

//       given up.Foo.ReplaceWith[Bar.type]()
//       given up.Ping.ReplaceWith[Ping.type]()
//     end L2
//     object L2 extends L2

//     // Error message should explain what doesn't match
//     // summon[Conversion[L2.Bar.T, L2.Ping.T]]

//     // val nc: L2.Ping.T = L2.Ping.NewCase(s = "hello", opt = None)
//     // println(nc)

//     // val pong: L2.Ping.T =
//     //   L2.Ping.Pong(k = 12, foo = L2.Bar(s = "x", opt = None))
//     // println(pong)

//     // nc.ex match
//     //   case L2.Ping.NewCase((s, opt)) => println(s"NewCase: $s")
//     //   case L2.Ping.Pong((k, foo))    => println(s"Pong: $k")
//     //   case L2.Ping.Bob((k, foo))     => println(s"Bob: $k")
//     // end match

//     // pong.ex match
//     //   case L2.Ping.NewCase((s, opt)) => println(s"NewCase: $s")
//     //   case L2.Ping.Pong((k, foo))    => println(s"Pong: $k")
//     //   case L2.Ping.Bob((k, foo))     => println(s"Bob: $k")
//     // end match
//   end innerSumTest
end Test
