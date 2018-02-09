package nl.biopet.tools.mergesv

case class Interval(start: Int, end: Int) {
  require(start <= end, "Start should be lower then end")
  def overlapWith(other: Interval): Boolean = {
    this.start <= other.end && other.start <= this.end
  }

  def extend(other: Interval): Option[Interval] = {
    if (this.overlapWith(other)) {
      Some(Interval(List(this.start, other.start).min, List(this.end, other.end).max))
    } else None
  }

  def shrink(other: Interval): Option[Interval] = {
    if (this.overlapWith(other)) {
      Some(Interval(List(this.start, other.start).max, List(this.end, other.end).min))
    } else None
  }

  def getMiddle: Int = {
    ((end - start) / 2) + start
  }
}
