/*
 * Copyright (c) 2018 Biopet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package nl.biopet.tools.mergesv

case class Interval(start: Int, end: Int) {
  require(start <= end, "Start should be lower then end")
  def overlapWith(other: Interval): Boolean = {
    this.start <= other.end && other.start <= this.end
  }

  def extend(other: Interval): Option[Interval] = {
    if (this.overlapWith(other)) {
      Some(
        Interval(List(this.start, other.start).min,
                 List(this.end, other.end).max))
    } else None
  }

  def shrink(other: Interval): Option[Interval] = {
    if (this.overlapWith(other)) {
      Some(
        Interval(List(this.start, other.start).max,
                 List(this.end, other.end).min))
    } else None
  }

  def getMiddle: Int = {
    ((end - start) / 2) + start
  }

  def relativeStart(pos: Int): Int = start - pos
  def relativeEnd(pos: Int): Int = end - pos
}
