import scala.annotation.tailrec
import scala.collection.immutable.BitSet

val MaxSurroundings = 4

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (result1, result2) = pickPapersUp(BitsetPapersMap.from(inputLines))

    (result1.toString, result2.toString)

end Solution

val range: Seq[Int] = -1 to 1
val around: Seq[(Int, Int)] = (for y <- range; x <- range yield (y, x)).filter(_ != (0, 0))

@tailrec
def pickPapersUp(papersMap: PapersMap, firstCleanO: Option[Int] = None, totalCleanings: Int = 0): (Int, Int) =
  val finalPapersMap = papersMap.elements.foldLeft(papersMap):
    case (cleaning, (row, col)) =>
      val papersAroundCount =
        around.count:
          (dy, dx) => papersMap.isPresent(row + dy)(col + dx)
      if papersAroundCount < MaxSurroundings then
        cleaning.remove(row)(col)
      else
        cleaning

  val cleanedThisTime = papersMap.count - finalPapersMap.count
  if cleanedThisTime == 0 then
    (firstCleanO.getOrElse(0), totalCleanings)
  else
    pickPapersUp(finalPapersMap, firstCleanO = firstCleanO.orElse(Some(cleanedThisTime)), totalCleanings + cleanedThisTime)

case class BitsetPapersMap(wideness: Int, data: BitSet) extends PapersMap:
  override def count: Int = data.size
  override def elements: Seq[(Int, Int)] =
    data.toSeq.map:
      value => (Math.floorDiv(value, wideness), value % wideness)

  override def isPresent(row: Int)(col: Int): Boolean = if row >= 0 && col >= 0 && col < wideness then data.contains(row*wideness + col) else false
  override def remove(row: Int)(col: Int): PapersMap = this.copy(data =  data - (row*wideness + col))

object BitsetPapersMap:
  def from(inputLines: Seq[String]): BitsetPapersMap =
    val wideness = inputLines.headOption.map(_.length).getOrElse(0)
    val values =
      for
        (line, rowIndex) <- inputLines.zipWithIndex
        (ch, positionIndex) <- line.zipWithIndex
        if ch == '@'
      yield rowIndex * wideness + positionIndex

    BitsetPapersMap(wideness, BitSet.fromSpecific(values))

trait PapersMap:
  def count: Int
  def elements: Seq[(Int, Int)]
  def isPresent(row: Int)(col: Int): Boolean
  def remove(row: Int)(col: Int): PapersMap