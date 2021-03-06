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

import nl.biopet.utils.conversions

object MergeMethod extends Enumeration {
  val Extend, Shrink = Value

  def mergeExtendCalls(calls: List[SvCall]): List[SvCall] = {
    calls
      .groupBy(_.svType)
      .flatMap {
        case (svType, list) =>
          list.foldLeft(List[SvCall]()) { (l, call) =>
            val overlap = l.groupBy(_.overlapWith(call))
            if (overlap.getOrElse(true, Nil).nonEmpty) {
              val overlapping = overlap(true)
              val posCi = overlapping
                .foldLeft(call.posCi)((a, b) => a.extend(b.posCi).getOrElse(a))
              val endCi = overlapping
                .foldLeft(call.endCi)((a, b) => a.extend(b.endCi).getOrElse(a))
              val callerFields = overlapping
                .map(_.callerFields)
                .foldLeft(call.callerFields.asInstanceOf[Map[String, Any]])(
                  (a: Map[String, Any], b: Map[String, Any]) =>
                    conversions.mergeMaps(a, b))
              val newCall = SvCall(
                call.contig1,
                posCi.getMiddle,
                call.contig2,
                endCi.getMiddle,
                svType,
                posCi,
                endCi,
                (call.callers ::: overlapping.flatMap(_.callers)).distinct,
                call.orientation1,
                call.orientation2,
                (call.existsInSamples ::: overlapping.flatMap(
                  _.existsInSamples)).distinct,
                callerFields.asInstanceOf[Map[String, Map[String, AnyRef]]]
              )
              newCall :: overlap.getOrElse(false, Nil)
            } else call :: l
          }
      }
      .toList
  }

}
