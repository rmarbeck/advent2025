import scala.annotation.tailrec

type Papers = Array[Array[Boolean]]
val MaxSurroundings = 4

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val originalPapers: Papers = inputLines.toArray.map:
      _.toArray.map:
        case '@' => true
        case _ => false

    val (result1, result2) = pickPapersUp(originalPapers)

    (result1.toString, result2.toString)

end Solution

val range: Seq[Int] = -1 to 1
val around: Seq[(Int, Int)] = (for y <- range; x <- range yield (y, x)).filter(_ != (0, 0))

@tailrec
def pickPapersUp(papers: Papers, firstDiff: Option[Int] = None, acc: Int = 0): (Int, Int) =
  val papersAfterPickup = Array.tabulate(papers.length, papers(0).length):
    case (y, x) => papers(y)(x) && ! isAvailableForCollect(y, x)(papers)

  val count = papers.map(_.count(_ == true)).sum
  val newCount = papersAfterPickup.map(_.count(_ == true)).sum
  val cleaned = count - newCount
  if cleaned == 0 then
    (firstDiff.getOrElse(0), acc)
  else
    pickPapersUp(papersAfterPickup, firstDiff = firstDiff.orElse(Some(cleaned)), acc + cleaned)


def isAvailableForCollect(row: Int, col: Int)(papers: Papers): Boolean =
  around.map:
    case (diffY, diffX) => if papers.isDefinedAt(row + diffY) && papers(row+diffY).isDefinedAt(col + diffX) then papers(row+diffY)(col+diffX) else false
  .count(_ == true) < MaxSurroundings