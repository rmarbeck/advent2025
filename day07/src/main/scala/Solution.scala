import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val manifold = inputLines.toArray.map(_.toCharArray)
    val beams = manifold.map:
      _.map:
        case 'S' => 1L
        case _ => 0L

    val start = manifold(0).indexWhere(_ == 'S')
    manifold(0)(start) = '|'

    val (result1, result2) = countSplits(manifold, beams)

    (result1.toString, result2.toString)

end Solution

@tailrec
def countSplits(remainingManifold: Array[Array[Char]], beamsPart2: Array[Array[Long]], splits: Int = 0): (Int, Long) =
  if (remainingManifold.length == 1)
    (splits, beamsPart2(0).sum)
  else
    val beamsPositionInPreviousLevel = beamsPart2(0).zipWithIndex.filter(_._1 > 0).map(_._2)
    val newSplits = beamsPositionInPreviousLevel.count(remainingManifold(1)(_) == '^')
    beamsPositionInPreviousLevel.foreach: beam =>
      if (remainingManifold(1)(beam) == '^')
        beamsPart2(1)(beam - 1) += beamsPart2(0)(beam)
        beamsPart2(1)(beam + 1) += beamsPart2(0)(beam)
      else
        beamsPart2(1)(beam) += beamsPart2(0)(beam)

    countSplits(remainingManifold.tail, beamsPart2.tail, newSplits + splits)
