import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val firstPositionOfSafe = 50

    val (result1, result2) = inputLines.iterator.scanLeft((firstPositionOfSafe, 0)):
      case ((safeFinalPosition, nbZeroesFromLastMove), ClickExtractor(clicks)) => positionTracker(safeFinalPosition, safeFinalPosition + clicks)
      case _ => throw Exception("not supported")
    .foldLeft((0, 0)):
        case ((zeroesInFinalPosition, zeroesPassedThrough), (0, zeroesPassedThroughThisMove)) => (zeroesInFinalPosition + 1, zeroesPassedThrough + zeroesPassedThroughThisMove)
        case ((zeroesInFinalPosition, zeroesPassedThrough), (_, zeroesPassedThroughThisMove)) => (zeroesInFinalPosition, zeroesPassedThrough + zeroesPassedThroughThisMove)

    (result1.toString, result2.toString)

end Solution

@tailrec
def modulo100Counter(newFinalPosition: Int, zeroesCounter: Int = 0): (Int, Int) =
  if newFinalPosition > 100 then
    modulo100Counter(newFinalPosition - 100, zeroesCounter + 1)
  else if newFinalPosition < 0 then
    modulo100Counter(newFinalPosition + 100, zeroesCounter + 1)
  else if newFinalPosition == 100 || newFinalPosition == 0 then
    (0, zeroesCounter + 1)
  else (newFinalPosition, zeroesCounter)


def positionTracker(positionBeforeMove: Int, orientedCLicks: Int): (Int, Int) =
  (positionBeforeMove, orientedCLicks) match
    case (0, value) if value < 0 => modulo100Counter(value, zeroesCounter = -1)
    case (_, value) => modulo100Counter(value, zeroesCounter = 0)


object ClickExtractor:
  def unapply(str: String): Option[Int] =
    str match
      case s"L${clicks}" => clicks.toIntOption.map(-_)
      case s"R${clicks}" => clicks.toIntOption
      case _  => None