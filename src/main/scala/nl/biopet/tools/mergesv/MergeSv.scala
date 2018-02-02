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

    val init = Init.createInit(cmdArgs)

    //TODO: Merging

    init.readers.foreach { case (key, list) => key -> list.foreach(_.close) }
    init.writer.close()
    logger.info("Done")
  }

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
