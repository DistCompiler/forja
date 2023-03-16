package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class UtilsTest extends AnyFunSuite {
  import Utils.*

  val moduleName = "TestModule"
  val testModule = s"module $moduleName"

  val testBasicDef = IR.Definition(
    name = "mt",
    params = List("_state1"),
    body = List(
      IR.Node.Name("_state1")
    )
  )
  val expectedBasicDef = s"""mt(_state1) ==\n_state1\n"""

  val testUninterpreted = List(
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
  val testMapOnSetNode: IR.Node.MapOnSet = IR.Node.MapOnSet(
    set = List(IR.Node.Name("_state1")),
    setMember = "l1",
    proc = testUninterpreted
  )
  val testLet: IR.Node.Let = IR.Node.Let(
    name = "_state2",
    binding = List(
      testMapOnSetNode
    ),
    body = List(IR.Node.Name("_state2"))
  )
  val testMapOnSetLetDef: IR.Definition = IR.Definition(
    name = "baz",
    params = List("_state1"),
    body = List(testLet)
  )
  val expectedUninterpreted = "[l1 EXCEPT !.y = l1.y - 1, !.x = l1.x + 1]"
  val expectedMapOnSetNode = s"{ $expectedUninterpreted: l1 \\in _state1 }"
  val expectedLet = s"LET _state2 == $expectedMapOnSetNode\nIN _state2\n"
  val expectedMapOnSetLetDef = s"""baz(_state1) ==\n$expectedLet\n"""

  val testFilterOnSetNode = IR.Definition(
    name = "testWait",
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
  val expectedFilterOnSetNode = "testWait(_state1) ==\nLET _state2 == { l1 \\in _state1: l1.x > 4 }\nIN _state2\n\n"

  List(
    testBasicDef -> expectedBasicDef,
    testMapOnSetLetDef -> expectedMapOnSetLetDef,
    testFilterOnSetNode -> expectedFilterOnSetNode
  ).foreach {
    case (input, expectedOutput) =>
      test(s"stringifyDefinition($input)") {
        val actualOutput = IRUtils.stringifyDefinition(
          definition = input
        ).mkString
        assert(actualOutput == expectedOutput)
      }
  }

  // From here onwards are unit tests which are indirectly covered by the tests above. TODO: Remove when done.

  List(
    testUninterpreted -> expectedUninterpreted,
    List(testMapOnSetNode) -> expectedMapOnSetNode,
    List(testLet) -> expectedLet
  ).foreach {
    case (input, expectedOutput) =>
      test(s"stringifyIRNodes($input)") {
        val actualOutput = IRUtils.stringifyNodes(
          nodes = input
        ).mkString
        assert(actualOutput == expectedOutput)
      }
  }

  List(
    testMapOnSetNode -> expectedMapOnSetNode,
    testLet -> expectedLet,
  ).foreach{
    case (input, expectedOutput) =>
      test(s"stringifyIRNode($input)") {
        val actualOutput = IRUtils.stringifyNode(input)
        assert(actualOutput.mkString == expectedOutput)
      }
  }

  List(
    Nil -> "",
    List("p") -> "p",
    List("_state1", "p1", "p2", "p3") -> "_state1, p1, p2, p3"
  ).foreach {
    case (input, expectedOutput) =>
      test(s"stringifyParams($input") {
        val actualOutput = IRUtils.stringifyParams(input)
        assert(actualOutput.mkString == expectedOutput)
      }
  }
}
