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

package forja.manip

import cats.syntax.all.given

import forja.*
import forja.dsl.*
import forja.source.{Source, SourceRange}

import scala.collection.IndexedSeqView

import SeqPattern.*

trait SeqPatternOps:
  def refine[T](manip: Manip[T]): SeqPattern[T] =
    SeqPattern:
      (SeqPattern.unit.manip, manip).mapN(_.withValue(_))

  @scala.annotation.targetName("deferSeqPattern")
  def defer[T](pattern: => SeqPattern[T]): SeqPattern[T] =
    SeqPattern(dsl.defer(pattern.manip))

  def field[T](pattern: SeqPattern[T]): SeqPattern[Fields[Tuple1[T]]] =
    pattern.map(t => Fields(Tuple1(t)))

  def fields[Tpl <: Tuple](
      pattern: SeqPattern[Tpl],
  ): SeqPattern[Fields[Tpl]] =
    pattern.map(Fields(_))

  def skip[T](pattern: SeqPattern[T]): SeqPattern[Fields[EmptyTuple]] =
    pattern.as(Fields(EmptyTuple))

  def anyNode(using DebugInfo): SeqPattern[Node] =
    SeqPattern:
      getNode.restrict:
        case node: Node =>
          Result.Match(node.parent.get, node.idxInParent, node)

  def anyChild(using DebugInfo): SeqPattern[Node.Child] =
    SeqPattern:
      getNode.restrict:
        case child: Node.Child =>
          Result.Match(child.parent.get, child.idxInParent, child)

  def tok(using DebugInfo)(tokens: Token*): SeqPattern[Node] =
    SeqPattern:
      val tokenSet = tokens.toSet
      getNode.restrict:
        case node: Node if tokenSet(node.token) =>
          Result.Match(node.parent.get, node.idxInParent, node)

  def atEnd(using DebugInfo): SeqPattern[Unit] =
    SeqPattern:
      getHandle.restrict:
        case Handle.Sentinel(parent, idx) =>
          Result.Look(parent, idx, ())

  def atBegin(using DebugInfo): SeqPattern[Unit] =
    SeqPattern:
      getHandle.restrict:
        case Handle.Sentinel(parent, 0) =>
          Result.Look(parent, 0, ())
        case Handle.AtChild(parent, 0, _) =>
          Result.Look(parent, 0, ())

  def nodeSpanMatchedBy(using
      DebugInfo.Ctx,
  )(
      pattern: SeqPattern[?],
  ): SeqPattern[IndexedSeqView[Node.Child]] =
    SeqPattern:
      getHandle
        .restrict:
          case handle: (Handle.Sentinel | Handle.AtChild) =>
            (handle.parent, handle.idx)
        .product(pattern.void.manip)
        .restrict:
          case (
                (parent, startIdx),
                patResult: (Result.Look[Unit] | Result.Match[Unit]),
              ) =>
            assert(patResult.parent eq parent)
            patResult.withValue:
              parent.children.view.slice(
                from = startIdx,
                until =
                  // Endpoint is exclusive.
                  // If we matched idx, add 1 to include it.
                  // Otherwise, default is fine.
                  if patResult.isMatch
                  then patResult.idx + 1
                  else patResult.idx,
              )

  export SeqPattern.{FieldsEndMarker as eof, FieldsTrailingMarker as trailing}

  def not[T](using DebugInfo.Ctx)(pattern: SeqPattern[T]): SeqPattern[Unit] =
    SeqPattern:
      ((pattern.manip *> Manip.pure(true)) | Manip.pure(false))
        .restrict:
          case false => ()
      *> SeqPattern.unit.manip

  def optional[T](using
      DebugInfo.Ctx,
  )(
      pattern: SeqPattern[T],
  ): SeqPattern[Option[T]] =
    pattern.map(Some(_)) | pure(None)

  def repeated[T](using
      DebugInfo.Ctx,
  )(
      pattern: SeqPattern[T],
  ): SeqPattern[List[T]] =
    lazy val impl: SeqPattern[List[T]] =
      (field(pattern) ~ field(defer(impl)) ~ trailing)
        .map(_ :: _)
        | pure(Nil)

    impl

  def repeated1[T](using
      DebugInfo.Ctx,
  )(
      pattern: SeqPattern[T],
  ): SeqPattern[List[T]] =
    (field(pattern) ~ field(repeated(pattern)) ~ trailing).map(_ :: _)

  def repeatedSepBy1[T](using
      DebugInfo.Ctx,
  )(sep: SeqPattern[?])(pattern: SeqPattern[T]): SeqPattern[List[T]] =
    (field(pattern) ~ field(
      repeated(skip(sep) ~ field(pattern) ~ trailing),
    ) ~ trailing)
      .map(_ :: _)

  def repeatedSepBy[T](using
      DebugInfo.Ctx,
  )(sep: SeqPattern[?])(
      pattern: SeqPattern[T],
  ): SeqPattern[List[T]] =
    repeatedSepBy1(sep)(pattern)
      | pure(Nil)

  def theTop(using DebugInfo): SeqPattern[Node.Top] =
    SeqPattern:
      getHandle.restrict:
        case Handle.AtTop(top) =>
          Result.Top(top, top)

  def theFirstChild(using DebugInfo): SeqPattern[Node.Child] =
    SeqPattern:
      getHandle.restrict:
        case Handle.AtChild(parent, 0, child) =>
          Result.Match(parent, 0, child)

  def children[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
    refine(atFirstChild(pattern.asManip))

  def onlyChild[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
    refine(atFirstChild((field(pattern) ~ eof).asManip))

  def parent[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
    refine(atParent(on(pattern).value))

  def ancestor[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
    refine(atAncestor(on(pattern).value))

  def lastChild[T](using DebugInfo)(pattern: SeqPattern[T]): SeqPattern[T] =
    refine(atLastChild(on(pattern).value))

  def embed[T: EmbedMeta](using DebugInfo): SeqPattern[T] =
    anyChild.restrict:
      case embed @ Node.Embed(t) if embed.meta == EmbedMeta[T] =>
        t.asInstanceOf[T]

  extension [P <: Node.Parent](parentPattern: SeqPattern[P])
    def withChildren[T](using
        DebugInfo,
    )(
        pattern: SeqPattern[T],
    ): SeqPattern[T] =
      SeqPattern:
        parentPattern.manip.lookahead.flatMap: result =>
          val parent = result.value
          atNode(parent)(atFirstChild(pattern.asManip))
            .map(result.withValue)

  extension (nodePattern: SeqPattern[Node])
    def src(using DebugInfo)(sourceRange: SourceRange): SeqPattern[Node] =
      nodePattern.filter(_.sourceRange == sourceRange)

    def src(using DebugInfo)(str: String): SeqPattern[Node] =
      src(SourceRange.entire(Source.fromString(str)))

  extension [T](hdPattern: SeqPattern[T])
    def *:[Tpl <: Tuple](tlPattern: SeqPattern[Tpl]): SeqPattern[T *: Tpl] =
      (hdPattern, tlPattern).mapN(_ *: _)
end SeqPatternOps
