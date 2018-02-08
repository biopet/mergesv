package nl.biopet.tools.mergesv

import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import nl.biopet.utils.ngs.vcf
import org.testng.annotations.Test

class SvCallTest extends BiopetTest {
  @Test
  def testBnd(): Unit = {
    val reader = new VCFFileReader(resourceFile("/bnd.vcf"), false)
    val variant = reader.iterator().next()
    reader.close()
    SvCall.from(variant, 0) shouldBe SvCall("chr2", 321681, "17", 198982, "BND", 0, 0, 0, 0)
  }
}
