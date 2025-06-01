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

package forja.langs.calc

import cats.syntax.all.given

import forja.*
import forja.Builtin.{Error, SourceMarker}
import forja.dsl.*
import forja.source.{Reader, SourceRange}
import forja.wf.Wellformed

import lang.*

object CalcReader extends Reader:
  import Reader.*
  object AddOp extends Token, Token.ShowSource
  object SubOp extends Token, Token.ShowSource
  object MulOp extends Token, Token.ShowSource
  object DivOp extends Token, Token.ShowSource
  object Group extends Token

  override lazy val wellformed = Wellformed:
    val content =
      repeated(choice(Expression, AddOp, SubOp, MulOp, DivOp, Group))
    Node.Top ::= content

    Number ::= Atom
    AddOp ::= Atom
    SubOp ::= Atom
    MulOp ::= Atom
    DivOp ::= Atom
    Group ::= content

    Expression ::= choice(Number)
  end wellformed

  private val digit: Set[Char] = ('0' to '9').toSet
  private val whitespace: Set[Char] = Set(' ', '\n', '\t')

  private lazy val unexpectedEOF: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(Error("unexpected EOF", SourceMarker(m)))
        *> Manip.pure(m)

  protected lazy val rules: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(whitespace):
          extendThisNodeWithMatch(rules)
        .onOneOf(digit):
          numberMode
        .on('+'):
          consumeMatch: m =>
            addChild(AddOp(m))
              *> rules
        .on('-'):
          consumeMatch: m =>
            addChild(SubOp(m))
              *> rules
        .on('*'):
          consumeMatch: m =>
            addChild(MulOp(m))
              *> rules
        .on('/'):
          consumeMatch: m =>
            addChild(DivOp(m))
              *> rules
        .on('('):
          consumeMatch: m =>
            addChild(Group(m))
              *> atFirstChild(rules)
        .on(')'):
          extendThisNodeWithMatch:
            atParent(rules)
            | consumeMatch: m =>
              addChild(Error("unexpected end of group", SourceMarker(m)))
                *> rules
        .fallback:
          bytes.selectOne:
            consumeMatch: m =>
              addChild(Error("invalid byte", SourceMarker(m)))
                *> rules
          | consumeMatch: m =>
            on(theTop).check
              *> Manip.pure(m)
          | unexpectedEOF

  private lazy val numberMode: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digit)(numberMode)
        .fallback:
          consumeMatch: m =>
            m.decodeString().toIntOption match
              case Some(value) =>
                addChild(
                  Expression(
                    Number(m),
                  ),
                )
                  *> rules
              case None =>
                addChild(Error("invalid number format", SourceMarker(m)))
                  *> rules
