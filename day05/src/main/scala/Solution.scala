object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (ranges, figures) = inputLines.foldLeft((Seq(), Seq()): (Seq[LongRange], Seq[Long])):
      case ((ranges, figures), s"$start-$end") => (LongRange(start.toLong, end.toLong) +: ranges, figures)
      case ((ranges, figures), s"$figure") if figure != "" => (ranges, figure.toLong +: figures)
      case (acc, _) => acc


    val result1 = figures.count: figure =>
      ranges.exists(_.contains(figure))

    val result2 = s""

    (result1.toString, result2.toString)

end Solution

case class LongRange(start: Long, end: Long):
  def contains(other: Long): Boolean = other >= start && other <= end