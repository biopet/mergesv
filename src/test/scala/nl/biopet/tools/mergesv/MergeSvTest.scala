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

import java.io.File

import nl.biopet.utils.ngs.intervals.BedRecord
import nl.biopet.utils.test.tools.ToolTest
import nl.biopet.utils.ngs.vcf
import nl.biopet.utils.io
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
    MergeSv.main(
      Array("-R",
            resourcePath("/fake_chrQ.fa"),
            "-o",
            outputFile.getAbsolutePath,
            "-i",
            "caller=" + resourcePath("/s1.vcf"),
            "-i",
            "caller=" + resourcePath("/s2.vcf")))
    vcf.getSampleIds(outputFile) shouldBe List("s1", "s2")

    MergeSv.main(
      Array("-R",
            resourcePath("/fake_chrQ.fa"),
            "-o",
            outputFile.getAbsolutePath,
            "-i",
            "caller=" + resourcePath("/s1.vcf"),
            "-i",
            "caller=" + resourcePath("/s1.vcf")))
    vcf.getSampleIds(outputFile) shouldBe List("s1")
  }

  @Test
  def testBnd(): Unit = {
    val outputFile = File.createTempFile("test.", ".vcf")
    outputFile.deleteOnExit()
    MergeSv.main(
      Array(
        "-R",
        resourcePath("/fake_chrQ.fa"),
        "-o",
        outputFile.getAbsolutePath,
        "-i",
        "caller=" + resourcePath("/s1.bnd.vcf"),
        "-i",
        "caller2=" + resourcePath("/s1.bnd.vcf"),
        "--keepNonVariant"
      ))
    vcf.getSampleIds(outputFile) shouldBe List("s1")
    val records = vcf.loadRegion(outputFile, BedRecord("chrQ", 1, 10000))
    records.size shouldBe 4
  }

  @Test
  def testList(): Unit = {
    val outputFile = File.createTempFile("test.", ".vcf")
    outputFile.deleteOnExit()
    val tsvFile = File.createTempFile("test.", ".tsv")
    tsvFile.deleteOnExit()
    io.writeLinesToFile(tsvFile,
                        List(s"caller\t${resourcePath("/s1.vcf")}",
                             s"caller\t${resourcePath("/s2.vcf")}"))
    MergeSv.main(
      Array("-R",
            resourcePath("/fake_chrQ.fa"),
            "--inputFileList",
            tsvFile.getAbsolutePath,
            "-o",
            outputFile.getAbsolutePath))
    vcf.getSampleIds(outputFile) shouldBe List("s1", "s2")
  }
}
