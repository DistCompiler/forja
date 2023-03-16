package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.Utils.IRUtils
import org.scalatest.funsuite.AnyFunSuite

class DCalCompilerTest extends AnyFunSuite {
  def executeTLA(testModule: String, testModuleName: String, testDefName: String,
                 initialStates: String, expectedStates: String): Unit = {
    val harness =
      s"""
         |---- MODULE Test ----
         |EXTENDS $testModuleName, TLC
         |
         |ASSUME PrintT(<<"initialStates", $initialStates>>) /\\ PrintT(<<"expectedStates", $expectedStates>>) /\\ $testDefName($initialStates) = $expectedStates
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

  final case class TLCTest(testDescription: String, module: String, defName: String,
                           initialStates: String, expectedStates: String)

  List(
    TLCTest(
      testDescription = "test AssignPairs",
      module =
        """module MyTest
          |def resetString() { str := "new string"; }""".stripMargin,
      defName = "resetString",
      initialStates =
        """{ [x |-> 0, y |-> 0, str |-> "", i |-> 1, set |-> {1, 5, 10}],
          |[x |-> 100, y |-> 0, str |-> "", i |-> 1, set |-> {0, 3, 6}],
          |[x |-> 0, y |-> 100, str |-> "", i |-> 1, set |-> {10, 11, 12}]}""".stripMargin,
      expectedStates =
        """{ [x |-> 0, y |-> 0, str |-> "new string", i |-> 1, set |-> {1, 5, 10}],
          |[x |-> 100, y |-> 0, str |-> "new string", i |-> 1, set |-> {0, 3, 6}],
          |[x |-> 0, y |-> 100, str |-> "new string", i |-> 1, set |-> {10, 11, 12}]}""".stripMargin
    )
  ).foreach {
    case TLCTest(testDescription, module, defName, initialStates, expectedStates) =>
      test(testDescription) {
        val compiledModule = IRBuilder(contents = module, fileName = "<filename>")
        val stringifiedModule = IRUtils.stringifyModule(compiledModule).mkString
        executeTLA(
          testModule = stringifiedModule,
          testModuleName = compiledModule.name,
          testDefName = defName,
          initialStates = initialStates,
          expectedStates = expectedStates
        )
      }
  }
}
