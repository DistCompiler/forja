package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestIRBuilder extends AnyFunSuite {
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
  
  val testDefWithStateReassignment = """def resetString() { str := "new string" }"""
  // Expected TLA+:
  //  resetString(_state1) ==
  //    LET
  //      _state2 == { [s EXCEPT !.str = "new string"]: s \in _state1 }
  //    IN
  //      _state2
  val expectedDefWithStateReassignment = IR.Definition(
    name = "resetString",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
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

  val testDefWithLet = s"def sum(p1, p2) { let local = p1 + p2 x := local }"
  // Expected TLA+:
  //  sum(_state1, p1, p2) ==
  //    LET
  //      _state2 == UNION {
  //          LET
  //            local == p1 + p2
  //          IN
  //            { [ss EXCEPT !.x = local] : ss \in { s } }
  //          : s \in _state1
  //      }
  //    IN
  //      _state2
  val expectedDefWithLet = IR.Definition(
    name = "sum",
    params = List("_state1", "p1", "p2"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.Uninterpreted("UNION { "),
          IR.Node.MapOnSet(
            set = List( IR.Node.Name("_state1") ),
            setMember = "s",
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
                  IR.Node.MapOnSet(
                    set = List(
                      IR.Node.Uninterpreted("{ "),
                      IR.Node.Name("s"),
                      IR.Node.Uninterpreted(" }")
                    ),
                    setMember = "ss",
                    proc = List(
                      IR.Node.Uninterpreted("["),
                      IR.Node.Name("ss"),
                      IR.Node.Uninterpreted(" EXCEPT !.x = "),
                      IR.Node.Name("local"),
                      IR.Node.Uninterpreted("]")
                    )
                  )
                )
              )
            )
          ),
          IR.Node.Uninterpreted("}")
        ),
        body = List(
          IR.Node.Name("_state2")
        )
      )
    )
  )

  val testDefWithLocal = s"def bar(v) { y := y - v i := i + 1 }"
  // Expected TLA+:
  //  change(_state1, v) ==
  //    LET
  //      _state2 == { [s EXCEPT !.y = s.y - v]: s \ in _state1 }
  //    IN
  //      LET
  //        _state3 == { [s EXCEPT !.i = s.i + 1]: s \ in _state2 }
  //      IN
  //        _state3
  val expectedDefWithLocal = IR.Definition(
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

  val testDefWithAssignPairs = s"def baz() { y := y - 1 || x := x + 1 }"
  val expectedDefWithAssignPairs = IR.Definition(
    name = "baz",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.MapOnSet(
            set = List(IR.Node.Name("_state1")),
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(".y"),
              IR.Node.Uninterpreted(" - "),
              IR.Node.Uninterpreted("1"),
              IR.Node.Uninterpreted(", "),
              IR.Node.Uninterpreted("!.x = "),
              IR.Node.Name("s"),
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

  val testMultiLineDef = s"def bar() { y := y - 1 x := x + 1 }"
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
            setMember = "s",
            proc = List(
              IR.Node.Uninterpreted("["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted(" EXCEPT "),
              IR.Node.Uninterpreted("!.y = "),
              IR.Node.Name("s"),
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
                setMember = "s",
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Name("s"),
                  IR.Node.Uninterpreted(" EXCEPT "),
                  IR.Node.Uninterpreted("!.x = "),
                  IR.Node.Name("s"),
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

  List(
    TestUtils.sequenceLines(testModule, testDefWithLet) -> IR.Module(
      name = moduleName,
      definitions = List(expectedDefWithLet)
    ),
    TestUtils.sequenceLines(testModule, testDefWithLocal) -> IR.Module(
      name = moduleName, definitions = List(expectedDefWithLocal)
    ),
    TestUtils.sequenceLines(
      testModule, testDefWithStateReassignment, testDefWithLocal, testDefNoParamsNoBody
    ) -> IR.Module(
      name = moduleName,
      definitions = List(expectedDefWithStateReassignment, expectedDefWithLocal, expectedDefNoParamsNoBody)
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
    TestUtils.sequenceLines(testModule, testDefWithStateReassignment) -> IR.Module(
      name = moduleName, definitions = List(expectedDefWithStateReassignment)
    ),
    TestUtils.sequenceLines(testModule, testDefWithAssignPairs) -> IR.Module(
      name = moduleName, definitions = List(expectedDefWithAssignPairs)
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
