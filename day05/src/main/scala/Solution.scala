import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (ranges, figures) = inputLines.foldLeft((Seq.empty[LongRange], Seq.empty[Long])):
      case ((ranges, figures), LongRangeExt(currentRange)) =>  (currentRange.mergeIn(ranges), figures)
      case ((ranges, figures), s"$figure") if figure != "" => (ranges, figure.toLong +: figures)
      case (acc, _) => acc

    val result1 = figures.count: figure =>
      ranges.exists(_.contains(figure))

    val result2 = ranges.map(_.count).sum

    (result1.toString, result2.toString)

end Solution

case class LongRange(start: Long, end: Long):
  def contains(other: Long): Boolean = other >= start && other <= end
  def count: Long = end - start + 1
  private def overlaps(otherRange: LongRange): Boolean = start <= otherRange.end + 1 && otherRange.start <= end + 1
  private def mergeWith(otherRange: LongRange) = LongRange(Math.min(start, otherRange.start), Math.max(end, otherRange.end))

  def mergeIn(alreadyMerged: Seq[LongRange]): Seq[LongRange] =
    val (toMergeWith, notTouched) = alreadyMerged.partition(_.overlaps(this))

    val mergedWith = toMergeWith.foldLeft(this):
      case (acc, merging) => acc.mergeWith(merging)

    notTouched :+ mergedWith

object LongRangeExt:
  def unapply(str: String): Option[LongRange] =
    str match
      case s"$start-$end" if start.toLongOption.isDefined && end.toLongOption.isDefined => Some(LongRange(start.toLong, end.toLong))
      case _ => None

