object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (ranges, figures) = inputLines.foldLeft((Seq(), Seq()): (Seq[LongRange], Seq[Long])):
      case ((ranges, figures), s"$start-$end") => (LongRange(start.toLong, end.toLong) +: ranges, figures)
      case ((ranges, figures), s"$figure") if figure != "" => (ranges, figure.toLong +: figures)
      case (acc, _) => acc


    val result1 = figures.count: figure =>
      ranges.exists(_.contains(figure))

    val result2 = merge(ranges).count

    (result1.toString, result2.toString)

end Solution

case class LongRange(start: Long, end: Long):
  def contains(other: Long): Boolean = other >= start && other <= end
  def overlaps(otherRange: LongRange): Boolean =
otherRange.contains(start) || otherRange.contains(end)
  def count: Long = end - start + 1
  def mergeWith(otherRange: LongRange) = LongRange(Math.min(start, otherRange.start), Math.max(end, otherRange.end))

def merge(toMerge: Seq[LongRange], merged: Seq[LongRange]): Seq[LongRange] =
  if toMerge.isEmpty then
    merged
  else
    val (head, tail) = (toMerge.head, toMerge.tail)
    val (notTouched, toMergeWith) = merged.partition(!_.contains(head))
    val mergedWithHead = toMergeWith.foldLeft(head):
    case (acc, toMergeWith) => acc.mergeWith(toMergeWith)
    merge(tail, notTouched ++ mergedWithHead)