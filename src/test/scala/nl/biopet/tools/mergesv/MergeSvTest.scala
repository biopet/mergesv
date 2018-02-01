package nl.biopet.tools.mergesv

import nl.biopet.utils.test.tools.ToolTest
import org.testng.annotations.Test

class MergeSvTest extends ToolTest[Args] {
  def toolCommand: MergeSv.type = MergeSv
  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      MergeSv.main(Array())
    }
  }
}
