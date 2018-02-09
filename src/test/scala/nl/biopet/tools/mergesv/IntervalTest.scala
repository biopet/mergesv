package nl.biopet.tools.mergesv

import nl.biopet.test.BiopetTest
import org.testng.annotations.Test

class IntervalTest extends  BiopetTest{
  @Test
  def testOverlap(): Unit = {
    Interval(1,2).overlapWith(Interval(3,4)) shouldBe false
    Interval(1,3).overlapWith(Interval(3,4)) shouldBe true
  }

  @Test
  def testExtend(): Unit = {
    Interval(1,2).extend(Interval(3,4)) shouldBe None
    Interval(1,3).extend(Interval(3,4)) shouldBe Some(Interval(1,4))
  }

  @Test
  def testShrink(): Unit = {
    Interval(1,2).shrink(Interval(3,4)) shouldBe None
    Interval(1,3).shrink(Interval(3,4)) shouldBe Some(Interval(3,3))
  }

  @Test
  def testMiddle(): Unit = {
    Interval(1,3).getMiddle shouldBe 2
    Interval(1,2).getMiddle shouldBe 1
  }
}
