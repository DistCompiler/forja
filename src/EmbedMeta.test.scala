// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja

import forja.dsl.*

class EmbedMetaTests extends munit.FunSuite:
  test("int == int"):
    assert(EmbedMeta[Int] == EmbedMeta[Int])

  test("int != long"):
    assert(EmbedMeta[Int] != EmbedMeta[Long])

  val n42 = Node.Embed(42)

  test("node with int"):
    assert(n42.meta == EmbedMeta[Int])

  test("pattern match"):
    // for parent purposes
    val top = Node.Top(n42)

    val manip =
      initNode(n42):
        on(embed[Int]).value

    assertEquals(manip.perform(), 42)

    // try rewriting, to be sure
    val manip2 =
      initNode(n42):
        pass(strategy = pass.bottomUp, once = true)
          .rules:
            on(embed[Int]).rewrite: i =>
              splice(Node.Embed(43))

    manip2.perform()

    assertEquals(top, Node.Top(Node.Embed(43)))

  // test("serialization"):
  /* val serialized =
   * Source.fromWritable(n42.toCompactWritable(Wellformed.empty)) */
  //   val tree = sexpr.parse.fromSourceRange(SourceRange.entire(serialized))
  //   val desern42 = Wellformed.empty.deserializeTree(tree)
  //   assertEquals(desern42, n42)

  // test("embed toString"):
  //   assertEquals(Node.Embed(42).toString(), "")
