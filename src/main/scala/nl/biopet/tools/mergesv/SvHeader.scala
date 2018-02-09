/*
 * Copyright (c) 2018 Sequencing Analysis Support Core - Leiden University Medical Center
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

import htsjdk.variant.vcf.{VCFFileReader, VCFHeader, VCFHeaderLine}
import nl.biopet.utils.io

import scala.collection.JavaConversions._

object SvHeader {
  def createSvHeader(samples: List[String]): VCFHeader = {
    val temp = File.createTempFile("header.", ".vcf")
    temp.deleteOnExit()
    io.writeLinesToFile(temp, svHeaderLines :: Nil)
    val r = new VCFFileReader(temp, false)
    val h = r.getFileHeader
    r.close()
    val lines: Set[VCFHeaderLine] =
      (h.getInfoHeaderLines ++ h.getFormatHeaderLines).toSet
    new VCFHeader(lines, samples)
  }

  /** This is a copy of the vcf 4.2 specs */
  def svHeaderLines: String =
    """##fileformat=VCFv4.2
      |##INFO=<ID=SVTYPE,Number=1,Type=String,Description="Type of structural variant">
      |##INFO=<ID=SVLEN,Number=.,Type=Integer,Description="Difference in length between REF and ALT alleles">
      |##INFO=<ID=CALLERS,Number=.,Type=String,Description="Caller that support this call">
      |##INFO=<ID=END,Number=1,Type=Integer,Description="End position of the variant described in this record">
      |##INFO=<ID=CIPOS,Number=2,Type=Integer,Description="Confidence interval around POS for imprecise variants">
      |##INFO=<ID=CIEND,Number=2,Type=Integer,Description="Confidence interval around END for imprecise variants">
      |##INFO=<ID=HOMLEN,Number=.,Type=Integer,Description="Length of base pair identical micro-homology at event breakpoints">
      |##INFO=<ID=HOMSEQ,Number=.,Type=String,Description="Sequence of base pair identical micro-homology at event breakpoints">
      |##INFO=<ID=BKPTID,Number=.,Type=String,Description="ID of the assembled alternate allele in the assembly file">
      |##INFO=<ID=MEINFO,Number=4,Type=String,Description="Mobile element info of the form NAME,START,END,POLARITY">
      |##INFO=<ID=METRANS,Number=4,Type=String,Description="Mobile element transduction info of the form CHR,START,END,POLARITY">
      |##INFO=<ID=DGVID,Number=1,Type=String,Description="ID of this element in Database of Genomic Variation">
      |##INFO=<ID=DBVARID,Number=1,Type=String,Description="ID of this element in DBVAR">
      |##INFO=<ID=DBRIPID,Number=1,Type=String,Description="ID of this element in DBRIP">
      |##INFO=<ID=MATEID,Number=.,Type=String,Description="ID of mate breakends">
      |##INFO=<ID=PARID,Number=1,Type=String,Description="ID of partner breakend">
      |##INFO=<ID=EVENT,Number=1,Type=String,Description="ID of event associated to breakend">
      |##INFO=<ID=CILEN,Number=2,Type=Integer,Description="Confidence interval around the inserted material between breakends">
      |##INFO=<ID=DP,Number=1,Type=Integer,Description="Read Depth of segment containing breakend">
      |##INFO=<ID=DPADJ,Number=.,Type=Integer,Description="Read Depth of adjacency">
      |##INFO=<ID=CN,Number=1,Type=Integer,Description="Copy number of segment containing breakend">
      |##INFO=<ID=CNADJ,Number=.,Type=Integer,Description="Copy number of adjacency">
      |##INFO=<ID=CICN,Number=2,Type=Integer,Description="Confidence interval around copy number for the segment">
      |##INFO=<ID=CICNADJ,Number=.,Type=Integer,Description="Confidence interval around copy number for the adjacency">
      |##FORMAT=<ID=CN,Number=1,Type=Integer,Description="Copy number genotype for imprecise events">
      |##FORMAT=<ID=CNQ,Number=1,Type=Float,Description="Copy number genotype quality for imprecise events">
      |##FORMAT=<ID=CNL,Number=G,Type=Float,Description="Copy number genotype likelihood for imprecise events">
      |##FORMAT=<ID=CNP,Number=G,Type=Float,Description="Copy number posterior probabilities">
      |##FORMAT=<ID=NQ,Number=1,Type=Integer,Description="Phred style probability score that the variant is novel">
      |##FORMAT=<ID=HAP,Number=1,Type=Integer,Description="Unique haplotype identifier">
      |##FORMAT=<ID=GT,Number=1,Type=String,Description="Genotype">
      |##FORMAT=<ID=AHAP,Number=1,Type=Integer,Description="Unique identifier of ancestral haplotype">""".stripMargin +
      "\n#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO"
}
