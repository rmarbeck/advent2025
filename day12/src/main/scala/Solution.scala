object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (shapes, _, constraints) = inputLines.filter(_.nonEmpty).foldLeft((Map.empty[Int, Shape], -1, Seq.empty[Constraint])):
      case ((shapes, currentId, constraints), constraint @ s"${height}x${width}: ${values}") => (shapes, currentId, Constraint(constraint) +: constraints)
      case ((shapes, _, constraints), s"${id}:") => (shapes + (id.toInt -> Shape(id.toInt, Seq.empty[String])), id.toInt, constraints)
      case ((shapes, currentId, constraints), line) => (shapes + (currentId -> shapes(currentId).add(line)), currentId, constraints)

    val sizes = shapes.map:
      case (key, shape) => key -> shape.blocks
    .toList.sortBy(_._1).map(_._2)

    val result1 = constraints.count(const => const.leftMember > const.rightMember(sizes))

    val result2 = "it s a win"

    (result1.toString, result2.toString)

end Solution

case class Shape(id: Int, lines: Seq[String]):
  def add(line: String): Shape = Shape(id, line +: lines)
  lazy val blocks: Int = lines.map(_.count(_ == '#')).sum

case class Constraint(input: String):
  private val (left, right) = input.span(_ != ' ')
  lazy val leftMember: Int = left match
    case s"${height}x${width}:" => height.toInt * width.toInt
    case _ => throw Exception("not supported")

  def rightMember(multipliers: Seq[Int]): Int =
    right.split(" ").filter(_.nonEmpty).zip(multipliers).map:
      case (operand, multiplier) => operand.toInt * multiplier
    .sum