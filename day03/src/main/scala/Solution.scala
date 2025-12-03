import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =


    val (result1, result2) = inputLines.map(findMax).reduce(_ + _)

    (result1.toString, result2.toString)

end Solution

def findMax(bank: String): (Long, Long) =
   val asDigits = bank.map(_.asDigit).toList
   (findMaxRec(asDigits, 2), findMaxRec(asDigits, 12))


@tailrec
def findMaxRec(bank: List[Int], remaining: Int = 2, found: Long = 0L): Int =
  remaining match
    case 0 => found 
    case _ => 
      val (first, indexFirst) = findFirstDigit(bank.dropRight(remaining - 1), 0, 0, 0)
      findMaxRec(bank.drop(indexFirst + 1), remaining - 1, found * 10 + first)


@tailrec
def findFirstDigit(bank: List[Int], best: Int, index: Int, bestIndex: Int): (Int, Int) =
  bank match
    case Nil => (best, bestIndex)
    case head :: tail if head == 9 => (9, index)
    case head :: tail if head > best => findFirstDigit(tail, head, index + 1, index)
    case head :: tail => findFirstDigit(tail, best, index + 1, bestIndex)

extension (tuple: (Long, Long))
  def +(other: (Long, Long)): (Long, Long) = (tuple._1 + other._1, tuple._2 + other._2)



