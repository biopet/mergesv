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

import htsjdk.samtools.reference.IndexedFastaSequenceFile
import htsjdk.variant.vcf.VCFFileReader
import nl.biopet.test.BiopetTest
import nl.biopet.utils.ngs.vcf
import org.testng.annotations.Test

class SvCallTest extends BiopetTest {
  @Test
  def testDel(): Unit = {
    val reader = new VCFFileReader(resourceFile("/del.vcf"), false)
    val it = reader.iterator()
    val v1 = it.next()
    val v2 = it.next()
    val v3 = it.next()
    it.close()
    reader.close()
    SvCall.from(v1, "caller", 0, Map()) shouldBe SvCall("chr2",
                                                        1000,
                                                        "chr2",
                                                        1500,
                                                        "DEL",
                                                        Interval(995, 1005),
                                                        Interval(1494, 1506),
                                                        "caller" :: Nil)
    SvCall.from(v2, "caller", 0, Map()) shouldBe SvCall("chr2",
                                                        1000,
                                                        "chr2",
                                                        1500,
                                                        "DEL",
                                                        Interval(995, 1005),
                                                        Interval(1495, 1505),
                                                        "caller" :: Nil)
    SvCall.from(v3, "caller", 0, Map()) shouldBe SvCall("chr2",
                                                        1000,
                                                        "chr2",
                                                        1500,
                                                        "DEL",
                                                        Interval(1000, 1000),
                                                        Interval(1500, 1500),
                                                        "caller" :: Nil)
    SvCall.from(v3, "caller", 3, Map()) shouldBe SvCall("chr2",
                                                        1000,
                                                        "chr2",
                                                        1500,
                                                        "DEL",
                                                        Interval(997, 1003),
                                                        Interval(1497, 1503),
                                                        "caller" :: Nil)
  }

  @Test
  def testBnd(): Unit = {
    val reader = new VCFFileReader(resourceFile("/bnd.vcf"), false)
    val it = reader.iterator()
    SvCall.from(it.next(), "caller", 0, Map()) shouldBe SvCall(
      "chr2",
      321681,
      "17",
      198982,
      "BND",
      Interval(321676, 321686),
      Interval(198976, 198988),
      "caller" :: Nil,
      true,
      false)
    SvCall.from(it.next(), "caller", 0, Map()) shouldBe SvCall(
      "chr2",
      321681,
      "17",
      198982,
      "BND",
      Interval(321676, 321686),
      Interval(198977, 198987),
      "caller" :: Nil,
      true,
      true)
    SvCall.from(it.next(), "caller", 0, Map()) shouldBe SvCall(
      "chr2",
      321681,
      "17",
      198982,
      "BND",
      Interval(321676, 321686),
      Interval(198977, 198987),
      "caller" :: Nil,
      false,
      false)
    SvCall.from(it.next(), "caller", 0, Map()) shouldBe SvCall(
      "chr2",
      321681,
      "17",
      198982,
      "BND",
      Interval(321676, 321686),
      Interval(198977, 198987),
      "caller" :: Nil,
      false,
      true)
    reader.close()
  }

  val call1 = SvCall("chr2",
                     321681,
                     "17",
                     198982,
                     "BND",
                     Interval(321676, 321686),
                     Interval(198976, 198988),
                     "caller" :: Nil)
  val call2 = SvCall("chr2",
                     321681,
                     "17",
                     198982,
                     "BND",
                     Interval(321676, 321686),
                     Interval(198976, 198988),
                     "caller" :: Nil)
  val call3 = SvCall("chr2",
                     321681,
                     "17",
                     198990,
                     "BND",
                     Interval(321676, 321686),
                     Interval(198984, 198996),
                     "caller" :: Nil)
  val call4 = SvCall("chr2",
                     321681,
                     "17",
                     198998,
                     "BND",
                     Interval(321676, 321686),
                     Interval(198992, 199004),
                     "caller" :: Nil)
  val call5 = SvCall("chr2",
                     321681,
                     "other",
                     198982,
                     "BND",
                     Interval(321676, 321686),
                     Interval(198976, 198988),
                     "caller" :: Nil)
  val call6 = SvCall("other",
                     321681,
                     "17",
                     198982,
                     "BND",
                     Interval(321676, 321686),
                     Interval(198976, 198988),
                     "caller" :: Nil)

  @Test
  def testOverlap(): Unit = {
    call1.overlapWith(call2) shouldBe true
    call1.overlapWith(call3) shouldBe true
    call1.overlapWith(call4) shouldBe false
    call1.overlapWith(call5) shouldBe false
    call1.overlapWith(call6) shouldBe false
  }

  @Test
  def testToVariantContext(): Unit = {
    val referenceReader = new IndexedFastaSequenceFile(
      resourceFile("/fake_chrQ.fa"))
    val context1 = SvCall("chrQ",
                          3000,
                          "chrQ",
                          4000,
                          "BND",
                          Interval(2950, 3050),
                          Interval(3950, 4050),
                          "caller" :: Nil).toVariantContext(referenceReader)
    context1.getAttribute("SVTYPE") shouldBe call1.svType
    referenceReader.close()
  }
}
