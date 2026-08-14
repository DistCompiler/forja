package forja.test

import forja.Lang2
import forja.Sum
import forja.Term
import forja.F
import forja.Lang2.SumMeta
import forja.Lang2.TermMeta

object Mini:
  trait L2 extends Lang2:
    class S derives Sum
    object S:
      abstract class Foo derives Term:
        def foo: F[String]
        def foo2: F[Byte]
      end Foo
      object Foo:
        abstract class Ping derives Term:
          def x: F[Bar]
        end Ping
      end Foo

      abstract class Bar derives Term
    end S
  end L2

  object L2 extends L2, Lang2.Impl[L2.type]

  trait L3 extends Lang2:
    object up extends L2, Lang2.Ext

    abstract class S derives Sum
    object S:
      export up.S.{Bar as _, *}
      abstract class Bar derives Term:
        def bar: F[Int]
      end Bar
    end S
  end L3

  object L3 extends L3, Lang2.Impl[L3.type]

  def main(args: Array[String]): Unit =
    import Lang2.{IsNode, LaunderNode}
    erased val a: IsNode.Aux[L2.S, L2.type, Tuple1["S"]] = summon[IsNode[L2.S]]

    // IsNode.instance[L3.S.Foo.Ping]
    erased val a1: IsNode.Aux[L3.S.Foo.Ping, L3.type, ("S", "Foo", "Ping")] = IsNode.instance[L3.S.Foo.Ping]
    erased val a2: IsNode.Aux[L3.up.S.Foo.Ping, L3.type, ("S", "Foo", "Ping")] = IsNode.instance[L3.up.S.Foo.Ping]
    erased val b: IsNode.Aux[L2.S.Foo, L2.type, ("S", "Foo")] = summon[IsNode[L2.S.Foo]]
    
    erased val c: LaunderNode.Aux[L3.type, L2.S, L3.S] = summon[LaunderNode[L3.type, L2.S]]
    erased val d: LaunderNode.Aux[L3.type, L2.S.Foo, L3.S.Foo] = summon[LaunderNode[L3.type, L2.S.Foo]]
    erased val e: LaunderNode.Aux[L3.type, L3.up.S.Foo.Ping, L3.S.Foo.Ping] = LaunderNode.instance[L3.type, L3.up.S.Foo.Ping]

    val _ : SumMeta[L2.S] {
      type Companion = L2.S.type
      type Cases = (L2.S.Bar, L2.S.Foo)
    } = summon[SumMeta[L2.S]]

    val _ : SumMeta[L3.S] {
      type Companion = L3.S.type
      type Cases = (L3.S.Bar, L3.S.Foo)
    } = summon[SumMeta[L3.S]]

    val _ : TermMeta[L2.S.Foo] {
      type Companion = L2.S.Foo.type
      type Cases = (String, Byte)
      type Labels = ("foo", "foo2")
    } = summon[TermMeta[L2.S.Foo]]

    // TODO: how to convince it to resolve extension methods on the companion object.
    // Clearly all the types are right...
    
    L2.S.Foo.meta

    val foo = L2.S.Foo.M("foo", 42)
    foo match
      case L2.S.Foo.M(a, b) =>
        println(s"foo $a, $b")

    assert(L2.S.meta.ordinal(L2.S.M(foo)) == 1)

    val _ : TermMeta[L3.S.Foo.Ping] {
      type Companion = L3.S.Foo.Ping.type
      type Cases = Tuple1[L3.S.Bar]
      type Labels = Tuple1["x"]
    } = summon[TermMeta[L3.S.Foo.Ping]]

    // summon[Lang2.SumMeta[L2.S]]
    // summon[TypeAdjust.Effective[L2.type, L2.S]]
    // L2.sumMeta[L2.S]
    // summon[Lang2.SumMeta[L3.S]]

    // This one fails because it's trying to read terms off the trait L3, not the object
    // erased val e: LaunderNode.Aux[L3, L2.S.Foo, L3.S.Foo] = LaunderNode.instance[L3, L2.S.Foo]

    // erased val a: Lang.EffectiveType.Aux[L.S.Foo.T, L.S.Foo.T] = Lang.effectiveNodeT[L.S.Foo.T]

    // erased val b: Lang.Sum.CaseList.Aux[L.S.type, Tuple1[L.S.Case.Foo.type]] = Lang.Sum.CaseList.instance[L.S.type]

    // erased val c: Lang.Sum.CaseListT.Aux[L.S.T, Tuple1[L.S.Foo.T]] = Lang.Sum.CaseListT.instance[L.S.T]

    // val _ : L.S.Foo.T = L.S.Foo("foo")
    // L.S.Foo("foo") match
    //   case L.S.Foo(f) => println(f)
    // end match
    // val _ : L.Bar.T = L.Bar()
    // L.Bar() match
    //   case L.Bar() => println("ok")
    // end match
    // // val _ : L.S.T = L.S.Foo("foo")
  end main
end Mini
