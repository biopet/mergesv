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

import htsjdk.samtools.SAMSequenceDictionary
import htsjdk.samtools.reference.IndexedFastaSequenceFile
import htsjdk.variant.variantcontext.writer.{
  VariantContextWriter,
  VariantContextWriterBuilder
}
import htsjdk.variant.vcf._
import nl.biopet.utils.ngs.fasta

import scala.collection.JavaConversions._

case class Init(dict: SAMSequenceDictionary,
                readers: Map[String, List[VCFFileReader]],
                headers: Map[String, List[VCFHeader]],
                samples: Seq[String],
                writer: VariantContextWriter,
                referenceReader: IndexedFastaSequenceFile)

object Init {
  def createInit(cmdArgs: Args): Init = {
    val dict = fasta.getCachedDict(cmdArgs.referenceFasta)

    val referenceReader = new IndexedFastaSequenceFile(cmdArgs.referenceFasta)

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

    val header = SvHeader.createSvHeader(samples)
    header.setSequenceDictionary(dict)

    for ((caller, fields) <- cmdArgs.callerFields) {
      require(headers.contains(caller), s"Caller '$caller' not found")
      for (field <- fields) {
        val oldLines =
          headers(caller).flatMap(x => Option(x.getFormatHeaderLine(field)))

        oldLines.headOption match {
          case Some(line) =>
            require(
              oldLines.map(_.getCountType).distinct.lengthCompare(1) == 0,
              s"for caller '$caller' field '$field' has different countTypes")
            require(oldLines.map(_.getType).distinct.lengthCompare(1) == 0,
                    s"for caller '$caller' field '$field' has different types")
            require(
              oldLines.map(_.getDescription).distinct.lengthCompare(1) == 0,
              s"for caller '$caller' field '$field' has different description")
            val newHeaderLine = new VCFFormatHeaderLine(s"$caller-$field",
                                                        line.getCountType,
                                                        line.getType,
                                                        line.getDescription)
            header.addMetaDataLine(newHeaderLine)
          case _ =>
            throw new IllegalArgumentException(
              s"For caller '$caller', field '$field' is not found")
        }
      }

    }
    writer.writeHeader(header)

    Init(dict, readers, headers, samples, writer, referenceReader)
  }
}
