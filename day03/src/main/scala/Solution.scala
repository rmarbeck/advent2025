import scala.annotation.tailrec

type BothResults = (Long, Long)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (result1, result2) = inputLines.map(findMax).reduce(_ + _)

    (result1.toString, result2.toString)

end Solution

def findMax(bank: String): (Long, Long) =
   val asDigits = bank.map(_.asDigit)
   (findMaxRec(asDigits, 2), findMaxRec(asDigits, 12))

@tailrec
def findMaxRec(bank: Seq[Int], remaining: Int, alreadyFound: Long = 0L): Long =
  remaining match
    case 0 => alreadyFound
    case _ => 
      val (best, indexFirst) = findBestFirstDigit(bank.dropRight(remaining - 1))
      findMaxRec(bank.drop(indexFirst + 1), remaining - 1, alreadyFound * 10 + best)

inline def findBestFirstDigit(bank: Seq[Int]): (Int, Int) = bank.zipWithIndex.maxBy(_._1)

extension (tuple: BothResults)
  def +(other: BothResults): BothResults = (tuple._1 + other._1, tuple._2 + other._2)



