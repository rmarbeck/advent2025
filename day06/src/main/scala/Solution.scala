import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val inLineNumbers = inputLines.toArray.map(_.split(" ").filter(_.nonEmpty)).transpose

    val result1 = inLineNumbers.map: row =>
      val values = row.init.map(_.toLong)
      row.last match
        case "+"      => values.sum
        case "*"      => values.product
    .sum

    val maxLengthForPadding = inputLines.view.init.map(_.length).max
    val operatorsAndSizes = extractOps(inputLines.last)
    val inColumnNumbers = inputLines.toArray.init.map(_.padTo(maxLengthForPadding, ' ').toCharArray).transpose

    val (_, result2) = operatorsAndSizes.foldLeft((0, 0L)):
      case ((currentStart, partialResult), (operatorAsChar, size)) =>
        val selectedNumbersByPosition = size match
          case -1     => inColumnNumbers.drop(currentStart)
          case value  => inColumnNumbers.slice(currentStart, currentStart + size)

        val numbers = selectedNumbersByPosition.map(_.mkString.trim.toLong)

        val currentResult = operatorAsChar match
          case '+'    => numbers.sum
          case '*'    => numbers.product

        (currentStart + size + 1, partialResult + currentResult)

    (result1.toString, result2.toString)

end Solution

@tailrec
def extractOps(fromString: String, partialResult: Seq[(Char, Int)] = Seq.empty[(Char, Int)]): Seq[(Char, Int)] =
  partialResult.headOption match
    case Some((_, -1))  => partialResult.reverse
    case _              =>
      val nextOpIndex = fromString.tail.indexWhere(_ != ' ')
      extractOps(fromString.drop(nextOpIndex + 1), (fromString.head, nextOpIndex) +: partialResult)
