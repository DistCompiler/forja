package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.Utils.IRUtils
import org.scalatest.funsuite.AnyFunSuite

class DCalCompilerTest extends AnyFunSuite {
  def executeTLA(testModule: String, testModuleName: String, testDefName: String, testParams: List[String] = Nil,
                 initialStates: String, expectedStates: String): Unit = {
    val call = if testParams == Nil then s"$testDefName($initialStates)" else s"$testDefName($initialStates, ${testParams.mkString(", ")})"
    printf("Calling %s\n", call)
    val harness =
      s"""
         |---- MODULE Test ----
         |EXTENDS $testModuleName, TLC
         |
         |ASSUME PrintT(<<"initialStates", $initialStates>>) /\\ PrintT(<<"expectedStates", $expectedStates>>) /\\ $call = $expectedStates
         |====
         |""".stripMargin

    val harnessCfg =
      """
        |
        |""".stripMargin

    val specDir = os.temp.dir()
    os.write(data = harness, target = specDir / "Test.tla")
    os.write(data = harnessCfg, target = specDir / "Test.cfg")
    os.write(data = testModule, target = specDir / s"$testModuleName.tla")

    os.proc("java", "-jar", os.pwd / "src" / "test" / "resources" / "tla2tools.jar", "Test.tla")
      .call(cwd = specDir)
  }

  final case class TLCTest(testDescription: String, module: String, defName: String, testParams: List[String] = Nil,
                           initialStates: String, expectedStates: String)

  val initialStates =
    """{ [x |-> 1, y |-> 3, str |-> "", i |-> 10, set |-> {1, 5, 10}],
      |[x |-> 19, y |-> 2, str |-> "", i |-> 100, set |-> {0, 3, 6}],
      |[x |-> 30, y |-> 1, str |-> "", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin

  List(
    TLCTest(
      testDescription = "Await",
      module =
        """module MyTest
          |def testWait() { await x > 4; }""".stripMargin,
      defName = "testWait",
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 19, y |-> 2, str |-> "", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 30, y |-> 1, str |-> "", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    ),
    TLCTest(
      testDescription = "AssignPairs",
      module =
        """module MyTest
          |def resetString() { str := "new string"; }""".stripMargin,
      defName = "resetString",
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 1, y |-> 3, str |-> "new string", i |-> 10, set |-> {1, 5, 10}],
          |[x |-> 19, y |-> 2, str |-> "new string", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 30, y |-> 1, str |-> "new string", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    ),
    TLCTest(
      testDescription = "long AssignPairs",
      module =
        """module MyTest
          |def baz() { y := y - 1 || x := x + 1; }""".stripMargin,
      defName = "baz",
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 2, y |-> 2, str |-> "", i |-> 10, set |-> {1, 5, 10}],
          |[x |-> 20, y |-> 1, str |-> "", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 31, y |-> 0, str |-> "", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    ),
    TLCTest(
      testDescription = "let = ...",
      module =
        """module MyTest
          |def sum(p1, p2) { let local = p1 + p2; x := local; }""".stripMargin,
      defName = "sum",
      testParams = List("3", "7"),
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 10, y |-> 3, str |-> "", i |-> 10, set |-> {1, 5, 10}],
          |[x |-> 10, y |-> 2, str |-> "", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 10, y |-> 1, str |-> "", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    ),
    TLCTest(
      testDescription = "IfThenElse",
      module =
        """module MyTest
          |def testIfThenElse() { if x <= y then { x := x + 1; } else { y := y - 1; } }""".stripMargin,
      defName = "testIfThenElse",
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 2, y |-> 3, str |-> "", i |-> 10, set |-> {1, 5, 10}],
          |[x |-> 19, y |-> 1, str |-> "", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 30, y |-> 0, str |-> "", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    )
  ).foreach {
    case TLCTest(testDescription, module, defName, params, initialStates, expectedStates) =>
      test(testDescription) {
        val compiledModule = IRBuilder(contents = module, fileName = "<filename>")
        val stringifiedModule = IRUtils.stringifyModule(compiledModule).mkString
        executeTLA(
          testModule = stringifiedModule,
          testModuleName = compiledModule.name,
          testDefName = defName,
          testParams = params,
          initialStates = initialStates,
          expectedStates = expectedStates
        )
      }
  }

  List(
    TLCTest(
      testDescription = "let \\in ...",
      module =
        """module MyTest
          |def ...""".stripMargin,
      defName = "???",
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 1, y |-> 3, str |-> "new string", i |-> 10, set |-> {1, 5, 10}],
          |[x |-> 19, y |-> 2, str |-> "new string", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 30, y |-> 1, str |-> "new string", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    ),
    TLCTest(
      testDescription = "var = ...",
      module =
        """module MyTest
          |def ...""".stripMargin,
      defName = "???",
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 1, y |-> 3, str |-> "new string", i |-> 10, set |-> {1, 5, 10}],
          |[x |-> 19, y |-> 2, str |-> "new string", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 30, y |-> 1, str |-> "new string", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    ),
    TLCTest(
      testDescription = "var \\in ...",
      module =
        """module MyTest
          |def ...""".stripMargin,
      defName = "???",
      initialStates = initialStates,
      expectedStates =
        """{ [x |-> 1, y |-> 3, str |-> "new string", i |-> 10, set |-> {1, 5, 10}],
          |[x |-> 19, y |-> 2, str |-> "new string", i |-> 100, set |-> {0, 3, 6}],
          |[x |-> 30, y |-> 1, str |-> "new string", i |-> 0, set |-> {10, 11, 12}] }""".stripMargin
    ),
  ).foreach {
    case TLCTest(testDescription, module, defName, params, initialStates, expectedStates) =>
      ignore(testDescription) {
        val compiledModule = IRBuilder(contents = module, fileName = "<filename>")
        val stringifiedModule = IRUtils.stringifyModule(compiledModule).mkString
        executeTLA(
          testModule = stringifiedModule,
          testModuleName = compiledModule.name,
          testDefName = defName,
          testParams = params,
          initialStates = initialStates,
          expectedStates = expectedStates
        )
      }
  }
}
