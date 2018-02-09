package nl.biopet.tools.mergesv

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
                .foldLeft(Option(call.posCi))((a, b) =>
                  a.flatMap(_.extend(b.posCi)))
                .get
              val endCi = overlapping
                .foldLeft(Option(call.endCi))((a, b) =>
                  a.flatMap(_.extend(b.endCi)))
                .get
              val newCall = SvCall(
                call.contig1,
                posCi.getMiddle,
                call.contig2,
                endCi.getMiddle,
                svType,
                posCi,
                endCi,
                (call.callers ::: overlapping.flatMap(_.callers)).distinct)
              newCall :: overlap.getOrElse(false, Nil)
            } else call :: l
          }
      }
      .toList
  }

}
