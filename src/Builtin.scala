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

import cats.syntax.all.given

object Builtin:
  object Error extends Token:
    def apply(msg: String, ast: Node.Child): Node =
      Error(
        Error.Message().at(msg),
        Error.AST(ast),
      )

    object Message extends Token:
      override val showSource = true
    object AST extends Token
  end Error

  object SourceMarker extends Token.ShowSource
end Builtin
