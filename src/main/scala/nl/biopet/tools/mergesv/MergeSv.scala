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

import htsjdk.samtools.SAMSequenceDictionary
import htsjdk.variant.variantcontext.writer.{
  VariantContextWriter,
  VariantContextWriterBuilder
}
import htsjdk.variant.vcf.{VCFFileReader, VCFHeader, VCFHeaderLine}
import nl.biopet.utils.ngs.{fasta, vcf}
import nl.biopet.utils.io
import nl.biopet.utils.tool.ToolCommand

import scala.collection.JavaConversions._

object MergeSv extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    val init = createInit(cmdArgs)

    //TODO: Merging

    init.readers.foreach { case (key, list) => key -> list.foreach(_.close) }
    init.writer.close()
    logger.info("Done")
  }

  protected case class Init(dict: SAMSequenceDictionary,
                            readers: Map[String, List[VCFFileReader]],
                            headers: Map[String, List[VCFHeader]],
                            samples: Seq[String],
                            writer: VariantContextWriter)

  def createInit(cmdArgs: Args): Init = {
    val dict = fasta.getCachedDict(cmdArgs.referenceFasta)

    val readers = cmdArgs.inputFiles.map {
      case (key, list) => key -> list.map(new VCFFileReader(_))
    }
    val headers = readers.map {
      case (key, list) => key -> list.map(_.getFileHeader)
    }

    val samples = headers
      .flatMap { case (_, l) => l.flatMap(_.getGenotypeSamples) }
      .toList
      .distinct
      .sorted

    val writer = new VariantContextWriterBuilder()
      .setOutputFile(cmdArgs.outputFile)
      .setReferenceDictionary(dict)
      .build

    val header = createSvHeader(samples)
    header.setSequenceDictionary(dict)
    writer.writeHeader(header)

    Init(dict, readers, headers, samples, writer)
  }

  private def createSvHeader(samples: List[String]): VCFHeader = {
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
  private def svHeaderLines: String =
    """##fileformat=VCFv4.2
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

  def descriptionText: String =
    """
      |This tool will merge structural variants from different callers and different samples.
      |Because SV's are not always exact on base resolution called the merging must be done within the windows what the callers say the SV's is placed.
    """.stripMargin

  def manualText: String =
    """
      |Each file can be tagged with a key, this can be used to see which callers does call the same event.
      |By default it try to stay withing the confident range the caller did specify.
    """.stripMargin

  def exampleText: String =
    s"""
      |Sample ID's in the input files should be correct, this is used in merging.
      |
      |Default example:
      |${example("-R",
                 "<reference_fasta>",
                 "-i",
                 "<caller>=<input_file>",
                 "-i",
                 "<caller>=<input_file>",
                 "-o",
                 "<output_file>")}
    """.stripMargin
}
