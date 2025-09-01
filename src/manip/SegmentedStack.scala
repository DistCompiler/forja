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

import scala.util.NotGiven

final class SegmentedStack[T <: AnyRef](using NotGiven[Array[?] <:< T]):
  inline val stackSegmentSize = 16
  private var stack = Array.ofDim[AnyRef](stackSegmentSize)
  private var base = stack
  private var stackSize = 0

  def consumeInReverse(fn: T => Unit): Unit =
    var idx = 0
    while base ne stack do
      base(idx) match
        case next: Array[AnyRef] if idx == 0 =>
          idx += 1 // back ref, skip
        case next: Array[AnyRef] if idx == base.length - 1 =>
          base = next
          idx = 0
        case elem =>
          fn(elem.asInstanceOf[T])
          idx += 1
    end while

    while idx < stackSize do
      base(idx) match
        case _: Array[AnyRef] if idx == 0 => // skip
        case elem =>
          fn(elem.asInstanceOf[T])
      base(idx) = null.asInstanceOf[AnyRef]
      idx += 1
    end while
    stackSize = 0
  end consumeInReverse

  def push(rec: T): Unit =
    if stackSize == stack.length
    then
      val oldStack = stack
      val prevElem = stack(stackSize - 1)
      stack = Array.ofDim[AnyRef](stackSegmentSize)
      oldStack(stackSize - 1) = stack
      stack(0) = oldStack
      stack(1) = prevElem
      stackSize = 2

    stack(stackSize) = rec
    stackSize += 1

  @scala.annotation.tailrec
  def pop(): T | Null =
    if stackSize == 0
    then null
    else
      stackSize -= 1
      stack(stackSize) match
        case lowerStack: Array[AnyRef] =>
          stack = lowerStack
          stack(stack.length - 1) = null.asInstanceOf[AnyRef] // drop next-ptr
          stackSize = stack.length - 1
          pop()
        case rec => rec.asInstanceOf[T]
end SegmentedStack
