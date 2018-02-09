package nl.biopet.tools.mergesv

import htsjdk.samtools.reference.IndexedFastaSequenceFile
import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import nl.biopet.utils.ngs.vcf
import org.testng.annotations.Test

class SvCallTest extends BiopetTest {
  @Test
  def testBnd(): Unit = {
    val reader = new VCFFileReader(resourceFile("/bnd.vcf"), false)
    val it = reader.iterator()
    SvCall.from(it.next(), "caller", 0) shouldBe SvCall("chr2", 321681, "17", 198982, "BND", Interval(321676, 321686), Interval(198976, 198988), "caller" :: Nil, true, false)
    SvCall.from(it.next(), "caller", 0) shouldBe SvCall("chr2", 321681, "17", 198982, "BND", Interval(321676, 321686), Interval(198977, 198987), "caller" :: Nil, true, true)
    SvCall.from(it.next(), "caller", 0) shouldBe SvCall("chr2", 321681, "17", 198982, "BND", Interval(321676, 321686), Interval(198977, 198987), "caller" :: Nil, false, false)
    SvCall.from(it.next(), "caller", 0) shouldBe SvCall("chr2", 321681, "17", 198982, "BND", Interval(321676, 321686), Interval(198977, 198987), "caller" :: Nil, false, true)
    reader.close()
  }

  @Test
  def testOverlap(): Unit = {
    val call1 = SvCall("chr2", 321681, "17", 198982, "BND", Interval(321676, 321686), Interval(198976, 198988), "caller" :: Nil)
    val call2 = SvCall("chr2", 321681, "17", 198982, "BND", Interval(321676, 321686), Interval(198976, 198988), "caller" :: Nil)
    val call3 = SvCall("chr2", 321681, "17", 198990, "BND", Interval(321676, 321686), Interval(198984, 198996), "caller" :: Nil)
    val call4 = SvCall("chr2", 321681, "17", 198998, "BND", Interval(321676, 321686), Interval(198992, 199004), "caller" :: Nil)
    val call5 = SvCall("chr2", 321681, "other", 198982, "BND", Interval(321676, 321686), Interval(198976, 198988), "caller" :: Nil)
    val call6 = SvCall("other", 321681, "17", 198982, "BND", Interval(321676, 321686), Interval(198976, 198988), "caller" :: Nil)
    call1.overlapWith(call2) shouldBe true
    call1.overlapWith(call3) shouldBe true
    call1.overlapWith(call4) shouldBe false
    call1.overlapWith(call5) shouldBe false
    call1.overlapWith(call6) shouldBe false
  }

  @Test
  def testToVariantContext(): Unit = {
    val referenceReader = new IndexedFastaSequenceFile(resourceFile("/fake_chrQ.fa"))
    val call1 = SvCall("chrQ", 3000, "chrQ", 4000, "BND", Interval(2950, 3050), Interval(3950, 4050), "caller" :: Nil)
    val context1 = call1.toVariantContext(referenceReader)
    context1.getAttribute("SVTYPE") shouldBe call1.svType
    referenceReader.close()
  }
}
