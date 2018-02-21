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
import htsjdk.variant.vcf.{
  VCFCompoundHeaderLine,
  VCFFileReader,
  VCFHeader,
  VCFHeaderLine
}
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
    //header.addMetaDataLine(VCFHeaderLine)
    writer.writeHeader(header)

    Init(dict, readers, headers, samples, writer, referenceReader)
  }
}
