package nl.biopet.tools.mergesv

import java.io.File

import nl.biopet.utils.ngs.intervals.BedRecord
import nl.biopet.utils.test.tools.ToolTest
import nl.biopet.utils.ngs.vcf
import org.testng.annotations.Test

class MergeSvTest extends ToolTest[Args] {
  def toolCommand: MergeSv.type = MergeSv

  @Test
  def testNoArgs(): Unit = {
    intercept[IllegalArgumentException] {
      MergeSv.main(Array())
    }
  }

  @Test
  def testEmptyFiles(): Unit = {
    val outputFile = File.createTempFile("test.", ".vcf")
    outputFile.deleteOnExit()
    MergeSv.main(Array("-R", resourcePath("/fake_chrQ.fa"), "-o", outputFile.getAbsolutePath,
      "-i", "caller=" + resourcePath("/s1.vcf"), "-i", "caller=" + resourcePath("/s2.vcf")))
    vcf.getSampleIds(outputFile) shouldBe List("s1", "s2")

    MergeSv.main(Array("-R", resourcePath("/fake_chrQ.fa"), "-o", outputFile.getAbsolutePath,
      "-i", "caller=" + resourcePath("/s1.vcf"), "-i", "caller=" + resourcePath("/s1.vcf")))
    vcf.getSampleIds(outputFile) shouldBe List("s1")
  }

  @Test
  def testBnd(): Unit = {
    val outputFile = File.createTempFile("test.", ".vcf")
    outputFile.deleteOnExit()
    MergeSv.main(Array("-R", resourcePath("/fake_chrQ.fa"), "-o", outputFile.getAbsolutePath,
      "-i", "caller=" + resourcePath("/s1.bnd.vcf"), "-i", "caller2=" + resourcePath("/s1.bnd.vcf"),
      "--keepNonVariant"))
    vcf.getSampleIds(outputFile) shouldBe List("s1")
    val records = vcf.loadRegion(outputFile, BedRecord("chrQ", 1, 10000))
    records.size shouldBe 4
  }
}
