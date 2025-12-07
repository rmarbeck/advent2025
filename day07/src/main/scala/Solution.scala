import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val manifold = inputLines.toArray.map(_.toCharArray)

    val start = manifold(0).indexWhere(_ == 'S')
    manifold(0)(start) = '|'

    val (result1, result2) = countSplits(manifold, 0, Seq(1))

    (result1.toString, result2.toString)

end Solution

@tailrec
def countSplits(remainingManifold: Array[Array[Char]], splits: Int, newPaths: Seq[Long]): (Int, Long) =
  if (remainingManifold.length == 1) {
    println(newPaths.filter(_ != 0L))
    (splits, newPaths.filter(_ != 0L).product)
  } else
    val beams = remainingManifold(0).zipWithIndex.filter(_._1 == '|').map(_._2)
    val newSplits = beams.count(remainingManifold(1)(_) == '^')
    beams.foreach: beam =>
      if (remainingManifold(1)(beam) == '^')
        remainingManifold(1)(beam - 1) = '|'
        remainingManifold(1)(beam + 1) = '|'
      else
        remainingManifold(1)(beam) = '|'

    val pathsEvo = remainingManifold(1).count(_ == '|') - remainingManifold(0).count(_ == '|')


    countSplits(remainingManifold.tail, newSplits + splits, pathsEvo +: newPaths)
