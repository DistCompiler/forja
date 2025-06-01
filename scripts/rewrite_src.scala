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
def rewrite_src_sc(): Unit =
  def cmd(parts: os.Shellable*): Unit =
    println(s"$$ ${parts.flatten(using _._1).mkString(" ")}")
    os.proc(parts*).call(cwd = os.pwd, stdout = os.Inherit, stderr = os.Inherit)

  cmd(
    "scala-cli",
    "run",
    ".",
    "--main-class",
    "scripts.update_license_sc",
    "--",
    "rewrite",
  )
  cmd("scala-cli", "fix", "--power", ".")
  cmd("scala-cli", "format", ".")
