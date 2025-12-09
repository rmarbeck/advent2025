import scala.collection.immutable.NumericRange

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val coords = inputLines.map:
      case s"$first,$last" => (first.toLong, last.toLong)

    val result1 = (coords.combinations(2).map:
      case Seq(first, last) => ((first._1 - last._1).abs + 1) * ((first._2 - last._2).abs + 1)
      ).toList.max

    val (cols, lines) =coords.foldLeft((Map.empty[Long, Set[Long]], Map.empty[Long, Set[Long]])):
      case ((cols, lines), (x, y)) =>
        (cols.updated(x, cols.getOrElse(x, Set()) + y), lines.updated(y, lines.getOrElse(y, Set()) + x))

    val greenCols =
      cols.map:
        case (k, values) => (k, values.min to values.max)

    val linesRange = coords.map(_._2).min to coords.map(_._2).max

    val paintedLines = linesRange.foldLeft(Map.empty[Long, NumericRange[Long]]):
      case (paintedLines, lineNumber) =>
        val colsOfLine = greenCols.filter(_._2.contains(lineNumber)).keys.toSeq
        paintedLines + (lineNumber -> (colsOfLine.min to colsOfLine.max))

    val inLine = lines.filter(_._2.size >= 2).flatMap:
      case (key, values: Set[Long]) => values.map((key, _))
    .toSet

    val inCols = cols.filter(_._2.size >= 2).flatMap:
      case (key, values: Set[Long]) => values.map((key, _))
    .toSet

    val validCoors = (inLine ++ inCols).filter(coords.contains)

    val candidates = (validCoors.toSeq.combinations(2).map:
      case Seq(first, last) => (first, last, ((first._1 - last._1).abs + 1) * ((first._2 - last._2).abs + 1))
      ).toList.sortBy(-_._3)

    val found = candidates.find:
      case (first, last, area) =>
        isFullPainted(first, last, paintedLines)


    println(found)

    val result2 = s""

    (result1.toString, result2.toString)

end Solution

def isFullPainted(first: (Long, Long), second: (Long, Long), paintedLines: Map[Long, NumericRange[Long]]): Boolean =
  val minX = math.min(first._1, second._1)
  val maxX = math.max(first._1, second._1)
  val minY = math.min(first._2, second._2)
  val maxY = math.max(first._2, second._2)

  (minY to maxY).forall:
    currentLine => paintedLines(currentLine).contains(minX) && paintedLines(currentLine).contains(maxX)

def updatePainted(current: Map[Long, NumericRange[Long]], newPaintings: Seq[(Long, NumericRange[Long])]): Map[Long, NumericRange[Long]] =
  if newPaintings.isEmpty then
    current
  else
    val currentPainting = newPaintings.head
    val inCurrent = current.getOrElse(currentPainting._1, currentPainting._2)

    current.updated(currentPainting._1, merge(inCurrent, currentPainting._2))

def merge(first: NumericRange[Long], second: NumericRange[Long]): NumericRange[Long] = {
  if (first.max < second.min || first.min > second.max) then
    println(s"merging $first with $second")
    throw Exception("Not supported")
  else
    math.min(first.min, second.min) to math.max(first.max, second.max)
}
