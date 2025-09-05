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

package scripts

@main
def check_no_diff_sc(): Unit =
  val result = os
    .proc("git", "status", "--porcelain")
    .call(cwd = os.pwd, mergeErrIntoOut = true)
  val strippedOut = result.out.text().strip()

  if strippedOut.isEmpty
  then
    println("ok: no diff detected")
    sys.exit(0)
  else
    println("diff detected!")
    strippedOut.linesIterator.foreach(println)
    sys.exit(1)
