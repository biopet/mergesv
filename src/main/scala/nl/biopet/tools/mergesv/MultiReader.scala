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

import nl.biopet.utils.ngs.vcf

import htsjdk.samtools.SAMSequenceRecord
import htsjdk.samtools.util.CloseableIterator
import htsjdk.variant.variantcontext.VariantContext

import scala.collection.JavaConversions._

class MultiReader(init: Init,
                  contig: SAMSequenceRecord,
                  defaultCi: Int,
                  keepNonVariants: Boolean)
    extends Iterator[SvCall]
    with AutoCloseable {

  protected val its: Map[String, List[CloseableIterator[VariantContext]]] =
    init.readers.map {
      case (caller, readers) =>
        caller -> readers.map(
          _.query(contig.getSequenceName, 1, contig.getSequenceLength))
    }
  protected val buffers: Map[String, Array[BufferedIterator[VariantContext]]] =
    its.map {
      case (caller, it) =>
        caller -> it
          .map(_.filter(_.getGenotypes.exists(g =>
            keepNonVariants || g.isCalled && !g.isHomRef)).buffered)
          .toArray
    }

  def hasNext: Boolean = buffers.exists { case (_, readers) => readers.exists(_.hasNext) }

  def headOption: Option[SvCall] = {
    if (hasNext) {
      val nextCalls = buffers.flatMap {
        case (caller, readers) =>
          readers.zipWithIndex
            .flatMap {
              case (buf, idx) =>
                if (buf.hasNext)
                  Some((caller, idx, buf.head))
                else None
            }
      }
      nextCalls.toList
        .sortBy { case (_, _, call) => call.getStart }
        .headOption
        .map { case (caller, idx, _) => SvCall.from(buffers(caller)(idx).head, caller, defaultCi) }
    } else None
  }

  var _total = 0L
  def total: Long = _total

  def next(): SvCall = {
    _total += 1
    val nextCalls = buffers.flatMap {
      case (caller, readers) =>
        readers.zipWithIndex
          .flatMap {
            case (buf, idx) =>
              if (buf.hasNext)
                Some((caller, idx, buf.head))
              else None
          }
    }
    nextCalls.toList
      .sortBy { case (_, _, call) => call.getStart }
      .headOption
      .map { case (caller, idx, _) => SvCall.from(buffers(caller)(idx).next(), caller, defaultCi) }
      .getOrElse(throw new IllegalStateException(
        "No records, please check .hasNext first"))
  }

  def close(): Unit = {
    its.foreach { case (_, readers) => readers.foreach(_.close()) }
  }
}
