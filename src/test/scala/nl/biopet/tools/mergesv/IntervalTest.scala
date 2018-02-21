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

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class IntervalTest extends BiopetTest {
  @Test
  def testOverlap(): Unit = {
    Interval(1, 2).overlapWith(Interval(3, 4)) shouldBe false
    Interval(1, 3).overlapWith(Interval(3, 4)) shouldBe true
  }

  @Test
  def testExtend(): Unit = {
    Interval(1, 2).extend(Interval(3, 4)) shouldBe None
    Interval(1, 3).extend(Interval(3, 4)) shouldBe Some(Interval(1, 4))
  }

  @Test
  def testShrink(): Unit = {
    Interval(1, 2).shrink(Interval(3, 4)) shouldBe None
    Interval(1, 3).shrink(Interval(3, 4)) shouldBe Some(Interval(3, 3))
  }

  @Test
  def testMiddle(): Unit = {
    Interval(1, 3).getMiddle shouldBe 2
    Interval(1, 2).getMiddle shouldBe 1
  }
}
