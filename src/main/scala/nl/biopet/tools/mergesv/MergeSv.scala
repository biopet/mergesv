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

import nl.biopet.utils.tool.ToolCommand

import scala.collection.JavaConversions._

object MergeSv extends ToolCommand[Args] {
  def emptyArgs = Args()
  def argsParser = new ArgsParser(this)

  def main(args: Array[String]): Unit = {
    val cmdArgs = cmdArrayToArgs(args)
    val numberInputs = cmdArgs.inputFiles.map { case (_, x) => x.size }.sum
    require(numberInputs > 1, "At least 2 input files required")

    logger.info("Start")

    val init = Init.createInit(cmdArgs)
    logger.info(s"Detected $numberInputs files")
    logger.info(s"Detected ${init.samples.size} samples")
    logger.info(s"Detected ${init.readers.keys.size} callers")

    slidingWindow(init,
                  cmdArgs.windowsSize,
                  cmdArgs.defaultCi,
                  cmdArgs.keepNonVariant,
                  cmdArgs.callerFields)

    init.readers.foreach { case (key, list) => key -> list.foreach(_.close) }
    init.writer.close()
    init.referenceReader.close()
    logger.info("Done")
  }

  def slidingWindow(init: Init,
                    windowSize: Int,
                    defaultCi: Int,
                    keepNonVariants: Boolean,
                    callerFields: Map[String, Set[String]]): Unit = {
    for (contig <- init.dict.getSequences) {
      logger.info(s"Starting on ${contig.getSequenceName}")
      val multiReader =
        new MultiReader(init, contig, defaultCi, keepNonVariants, callerFields)

      val writeCount = readWrite(multiReader, windowSize, init)

      logger.info(
        multiReader.total + s" variants found on ${contig.getSequenceName}")
      logger.info(
        writeCount + s" variants writen for ${contig.getSequenceName}")

      multiReader.close()
    }
  }

  private def writeToFile(buf: List[SvCall], init: Init): Long = {
    buf.sortBy(_.pos1).foreach { c =>
      init.writer.add(c.toVariantContext(init.referenceReader))
    }
    buf.size
  }

  private def readWrite(multiReader: MultiReader,
                        windowSize: Int,
                        init: Init): Long = {
    var writeCount = 0L

    if (multiReader.hasNext) {
      val f = multiReader.next()
      val (_, remain) =
        multiReader.foldLeft((f, List(f))) {
          case ((first: SvCall, buf), record) =>
            if (record.pos1 <= (first.pos1 + windowSize)) (first, record :: buf)
            else {
              val newBuf =
                MergeMethod.mergeExtendCalls(buf)
              val split = newBuf.groupBy(
                _.pos1 >= (multiReader.headOption
                  .map(_.pos1)
                  .getOrElse(0) - windowSize))
              writeCount += writeToFile(split.getOrElse(false, Nil), init)
              val keep = split.getOrElse(true, Nil)

              (keep.sortBy(_.pos1).headOption.getOrElse(record), record :: keep)
            }
        }
      writeCount += writeToFile(MergeMethod.mergeExtendCalls(remain), init)
    }
    writeCount
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
