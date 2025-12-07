import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val manifold = inputLines.toArray.map(_.toCharArray)
    val beams = manifold.map:
      _.map:
          case 'S'  => 1L
          case _    => 0L

    val (result1, result2) = countSplitsAndPaths(manifold, beams)

    (result1.toString, result2.toString)

end Solution

@tailrec
def countSplitsAndPaths(remainingManifold: Array[Array[Char]], beams: Array[Array[Long]], splits: Int = 0): (Int, Long) =
  if (remainingManifold.length == 1)
    (splits, beams(0).sum)
  else
    val beamsPositionInPreviousLevel = beams(0).zipWithIndex.filter(_._1 > 0).map(_._2)
    val newSplits =
      beamsPositionInPreviousLevel.foldLeft(0): (acc, beam) =>
        if remainingManifold(1)(beam) == '^' then
          beams(1)(beam - 1) += beams(0)(beam)
          beams(1)(beam + 1) += beams(0)(beam)
          acc + 1
        else
          beams(1)(beam) += beams(0)(beam)
          acc

    countSplitsAndPaths(remainingManifold.tail, beams.tail, newSplits + splits)
