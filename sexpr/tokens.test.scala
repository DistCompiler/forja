// Copyright 2024-2025 DCal Team
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

package distcompiler.sexpr

class tokensTest extends munit.FunSuite:
  test("token names"):
    assertEquals(lang.Atom.name, "distcompiler.sexpr.lang.Atom")
    assertEquals(lang.List.name, "distcompiler.sexpr.lang.List")
