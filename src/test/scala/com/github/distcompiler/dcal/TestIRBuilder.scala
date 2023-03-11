package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestIRBuilder extends AnyFunSuite {
  import IRBuilder.*

  val moduleName = "TestModule"
  val testModule = s"module $moduleName"

  val testDefNoParamsNoBody = "def mt() {}"
  val expectedDefNoParamsNoBody = IR.Definition(
    name = "mt",
    params = List("_state1"),
    body = List(
      IR.Node.Name("_state1")
    )
  )

  val testStateAssignPairs = """def resetString() { str := "new string"; }"""
  // Expected TLA+:
  //  resetString(_state1) ==
  //    LET
  //      _state2 == { [s EXCEPT !.str = "new string"]: s \in _state1 }
  //    IN
  //      _state2
  val expectedStateAssignPairs = IR.Definition(
    name = "resetString",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.str = "),
              IR.Node.Uninterpreted(""""new string""""),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(
          IR.Node.Name("_state2")
        )
      )
    )
  )

  val testLongAssignPairs = "def baz() { y := y - 1 || x := x + 1; }"
  val expectedLongAssignPairs = IR.Definition(
    name = "baz",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("!.x = "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" + "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(IR.Node.Name("_state2"))
      )
    )
  )

  val testLet = "def sum(p1, p2) { let local = p1 + p2; x := local; }"
  val expectedLet = IR.Definition(
    name = "sum",
    params = List("_state1", "p1", "p2"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.Uninterpreted("UNION"),
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "l1",
            proc = List(
              IR.Node.Let(
                name = "local",
                binding = List(
                  IR.Node.Name("p1"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Name("p2")
                ),
                // { [ss EXCEPT !.x = local] : ss \in { s } }
                body = List(
                  IR.Node.Let(
                    name = "_state3",
                    binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name("l1"), IR.Node.Uninterpreted(" }")),
                    body = List(
                      IR.Node.Let(
                        name = "_state4",
                        binding = List(
                          IR.Node.MapOnSet(
                            set = List(IR.Node.Name("_state3")),
                            setMember = "l2",
                            proc = List(
                              IR.Node.Uninterpreted("["),
                              IR.Node.Name("l2"),
                              IR.Node.Uninterpreted(" EXCEPT "),
                              IR.Node.Uninterpreted("!.x = "),
                              IR.Node.Name("local"),
                              IR.Node.Uninterpreted("]")
                            )
                          )
                        ),
                        body = List(IR.Node.Name("_state4"))
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        body = List(
          IR.Node.Name("_state2")
        )
      )
    )
  )

  val testDefParam = "def bar(v) { y := y - v; i := i + 1; }"
  // Expected TLA+:
  //  change(_state1, v) ==
  //    LET
  //      _state2 == { [s EXCEPT !.y = s.y - v]: s \ in _state1 }
  //    IN
  //      LET
  //        _state3 == { [s EXCEPT !.i = s.i + 1]: s \ in _state2 }
  //      IN
  //        _state3
  val expectedDefParam = IR.Definition(
    name = "bar",
    params = List("_state1", "v"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        // { [s EXCEPT !.y = s.y - v]: s \in _state1 }
        binding = List(
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Name("v"),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            // { [s EXCEPT !.i = s.i + 1]: s \in _state2 }
            binding = List(
              IR.Node.MapOnSet(
                set = List( IR.Node.Name("_state2") ),
                setMember = "s",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("s"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.i = "),
                  IR.Node.Name("s"),
                  IR.Node.Uninterpreted(".i"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Uninterpreted("1"),
                  IR.Node.Uninterpreted("]")
                )
              )
            ),
            body = List(
              IR.Node.Name("_state3")
            )
          )
        )
      )
    )
  )

  val testMultiLineDef = "def bar() { y := y - 1; x := x + 1; }"
  val expectedMultiLineDef = IR.Definition(
    name = "bar",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        // { [s EXCEPT !.y = s.y - 1]: s \in _state1 }
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted("]")
            )
          )
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            // { [s EXCEPT !.x = s.x + 1]: s \in _state2 }
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "l2",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("l2"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.x = "),
                  IR.Node.Name("l2"),
                  IR.Node.Uninterpreted(".x"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Uninterpreted("1"),
                  IR.Node.Uninterpreted("]")
                )
              )
            ),
            body = List(
              IR.Node.Name("_state3")
            )
          )
        )
      )
    )
  )

  val testVar = "def testVar() { var z = 10; x := x + z; }"
  val expectedVar = IR.Definition(
    name = "testVar",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Uninterpreted("l2"), // TODO: Ask if this should be name or uninterpreted
              IR.Node.Uninterpreted(" \\in DOMAIN "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted("""\cup { "z" } |-> IF """),
              // Because only from "|->" onwards is l2 defined
              IR.Node.Uninterpreted("l2"),
              IR.Node.Uninterpreted(""" = "z" THEN """),
              IR.Node.Uninterpreted("10"),
              IR.Node.Uninterpreted(" ELSE "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted("["),
              IR.Node.Uninterpreted("l2"),
              IR.Node.Uninterpreted("]]")
            )
          )
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "l3",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("l3"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.x = "),
                  IR.Node.Name("l3"),
                  IR.Node.Uninterpreted(".x"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Name("l3"),
                  IR.Node.Uninterpreted(".z"),
                  IR.Node.Uninterpreted("]"),
                )
              )
            ),
            body = List(IR.Node.Name("_state3"))
          )
        )
      )
    )
  )

  val testIfThenElse = "def branch() { if x <= y then { x := x + 1; } else { y := y - 1; } i := x + y; }"
  val expectedIfThenElse = IR.Definition(
    name = "branch",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.Uninterpreted("UNION "),
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            proc = List(
              IR.Node.Uninterpreted("IF "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" <= "),
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".y"),
              // TODO: Possibly add a whitespace or newline here, between IF ... THEN ... ELSE?
              IR.Node.Uninterpreted("THEN "),
              IR.Node.Let(
                name = "_state3",
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" }")
                ),
                body = List(
                  IR.Node.Let(
                    name = "_state4",
                    binding = List(
                      IR.Node.MapOnSet(
                        set = List(IR.Node.Name("_state3")),
                        setMember = "l2",
                        proc = List(
                          IR.Node.Uninterpreted("["),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(" EXCEPT "),
                          IR.Node.Uninterpreted("!.x = "),
                          IR.Node.Name("l2"),
                          IR.Node.Uninterpreted(".x"),
                          IR.Node.Uninterpreted(" + "),
                          IR.Node.Uninterpreted("1"),
                          IR.Node.Uninterpreted("]")
                        )
                      )
                    ),
                    body = List(IR.Node.Name("_state4"))
                  )
                )
              ),
              IR.Node.Uninterpreted("ELSE "),
              IR.Node.Let(
                name = "_state5",
                binding = List(
                  IR.Node.Uninterpreted("{ "),
                  IR.Node.Name("l1"),
                  IR.Node.Uninterpreted(" }")
                ),
                body = List(
                  IR.Node.Let(
                    name = "_state6",
                    binding = List(
                      IR.Node.MapOnSet(
                        set = List(IR.Node.Name("_state5")),
                        setMember = "l3",
                        proc = List(
                          IR.Node.Uninterpreted("["),
                          IR.Node.Name("l3"),
                          IR.Node.Uninterpreted(" EXCEPT "),
                          IR.Node.Uninterpreted("!.y = "),
                          IR.Node.Name("l3"),
                          IR.Node.Uninterpreted(".y"),
                          IR.Node.Uninterpreted(" - "),
                          IR.Node.Uninterpreted("1"),
                          IR.Node.Uninterpreted("]")
                        )
                      )
                    ),
                    body = List(
                      IR.Node.Name("_state6")
                    )
                  )
                )
              )
            )
          )
        ),
        body = List(
          IR.Node.Let(
            name = "_state7",
            binding = List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name("_state2")),
                setMember = "l4",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("l4"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.i = "),
                  IR.Node.Name("l4"),
                  IR.Node.Uninterpreted(".x"),
                  IR.Node.Uninterpreted(" + "),
                  IR.Node.Name("l4"),
                  IR.Node.Uninterpreted(".y"),
                  IR.Node.Uninterpreted("]")
                )
              )
            ),
            body = List(IR.Node.Name("_state7"))
          )
        )
      )
    )
  )

  val testAwait = "def wait() { await x > 4; }"
  val expectedAwait = IR.Definition(
    name = "wait",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.FilterOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "l1",
            pred = List(
              IR.Node.Name("l1"),
              IR.Node.Uninterpreted(".x"),
              IR.Node.Uninterpreted(" > "),
              IR.Node.Uninterpreted("4")
            )
          )
        ),
        body = List(IR.Node.Name("_state2"))
      )
    )
  )

  List(
    TestUtils.sequenceLines(testModule, testDefParam) -> IR.Module(
      name = moduleName, definitions = List(expectedDefParam)
    ),
    TestUtils.sequenceLines(
      testModule, testStateAssignPairs, testDefParam, testDefNoParamsNoBody
    ) -> IR.Module(
      name = moduleName,
      definitions = List(expectedStateAssignPairs, expectedDefParam, expectedDefNoParamsNoBody)
    )
  ).foreach {
    case (input, expectedOutput) =>
      ignore(s"generateIR($input)") {
        val actualOutput = IRBuilder(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualOutput == expectedOutput)
      }
  }

  List(
    testModule -> IR.Module(
      name = moduleName, definitions = Nil
    ),
    TestUtils.sequenceLines(testModule, testDefNoParamsNoBody) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedDefNoParamsNoBody
      )
    ),
    TestUtils.sequenceLines(testModule, testStateAssignPairs) -> IR.Module(
      name = moduleName, definitions = List(expectedStateAssignPairs)
    ),
    TestUtils.sequenceLines(testModule, testLongAssignPairs) -> IR.Module(
      name = moduleName, definitions = List(expectedLongAssignPairs)
    ),
    TestUtils.sequenceLines(testModule, testIfThenElse) -> IR.Module(
      name = moduleName, definitions = List(expectedIfThenElse)
    ),
    TestUtils.sequenceLines(testModule, testLet) -> IR.Module(
      name = moduleName,
      definitions = List(expectedLet)
    ),
    TestUtils.sequenceLines(testModule, testAwait) -> IR.Module(
      name = moduleName, definitions = List(expectedAwait)
    ),
    TestUtils.sequenceLines(testModule, testVar) -> IR.Module(
      name = moduleName, definitions = List(expectedVar)
    ),
    TestUtils.sequenceLines(testModule, testMultiLineDef) -> IR.Module(
      name = moduleName, definitions = List(expectedMultiLineDef)
    )
  ).foreach {
    case (input, expectedOutput) =>
      test(s"generateIR($input)") {
        val actualOutput = IRBuilder(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualOutput == expectedOutput)
      }
  }
}
