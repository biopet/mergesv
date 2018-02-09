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
import scala.collection.mutable.ListBuffer

object MergeSv extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)

    logger.info("Start")

    val init = Init.createInit(cmdArgs)

    slidingWindow(init,
                  cmdArgs.windowsSize,
                  cmdArgs.defaultCi,
                  cmdArgs.keepNonVariant)

    init.readers.foreach { case (key, list) => key -> list.foreach(_.close) }
    init.writer.close()
    init.referenceReader.close()
    logger.info("Done")
  }

  def slidingWindow(init: Init,
                    windowSize: Int,
                    defaultCi: Int,
                    keepNonVariants: Boolean): Unit = {
    for (contig <- init.dict.getSequences) {
      logger.info(s"Starting on ${contig.getSequenceName}")
      val multiReader =
        new MultiReader(init, contig, defaultCi, keepNonVariants)

      var writeCount = 0L
      def readWrite(buf: List[SvCall] = Nil): Unit = {

        if (multiReader.hasNext) {
          val split = buf.groupBy(
            _.pos1 >= (multiReader.headOption.get.pos1 - windowSize))
          val write = split.getOrElse(false, Nil).sortBy(_.pos1)
          write.foreach { c =>
            writeCount += 1
            init.writer.add(c.toVariantContext(init.referenceReader))
          }
          val keep = split.getOrElse(true, Nil)
          val end =
            if (keep.isEmpty) multiReader.headOption.get.pos1 + windowSize
            else keep.map(_.pos1).min + windowSize
          val newCalls = {
            val b = new ListBuffer[SvCall]
            while (multiReader.headOption.exists(_.pos1 <= end)) b.append(
              multiReader.next())
            b.toList
          }
          val newBuf = MergeMethod.mergeExtendCalls(
            newCalls ::: split.getOrElse(true, Nil))
          readWrite(newBuf)
        } else {
          buf.sortBy(_.pos1).foreach { c =>
            writeCount += 1
            init.writer.add(c.toVariantContext(init.referenceReader))
          }
        }
      }

      readWrite()

      logger.info(
        multiReader.total + s" variants found on ${contig.getSequenceName}")
      logger.info(
        writeCount + s" variants writen for ${contig.getSequenceName}")

      multiReader.close()
    }
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
