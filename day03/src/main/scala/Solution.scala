import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =


    val result1 = inputLines.map(findMax).sum

    val result2 = s""

    (result1.toString, result2.toString)

end Solution

def findMax(bank: String): Int =
  val (first, indexFirst) = findFirstDigit(bank.map(_.asDigit).dropRight(1).toList, 0, 0, 0)
  val second = bank.drop(indexFirst + 1).map(_.asDigit).max
  first * 10 + second


@tailrec
def findFirstDigit(bank: List[Int], best: Int, index: Int, bestIndex: Int): (Int, Int) =
  bank match
    case Nil => (best, bestIndex)
    case head :: tail if head == 9 => (9, index)
    case head :: tail if head > best => findFirstDigit(tail, head, index + 1, index)
    case head :: tail => findFirstDigit(tail, best, index + 1, bestIndex)




