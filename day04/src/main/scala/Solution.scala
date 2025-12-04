object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given papers: Array[Array[Boolean]] = inputLines.toArray.map:
      line => line.toArray.map:
        case '@' => true
        case _ => false


    val result1 = (for
      y <- papers.indices
      x <- papers(0).indices
      if papers(y)(x)
    yield
      isAvailableForCollect(y, x)
    ).count(_ == true)



    val result2 = s""

    (result1.toString, result2.toString)

end Solution

val around = List((-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0))

def isAvailableForCollect(row: Int, col: Int)(using papers: Array[Array[Boolean]]): Boolean =
  around.map:
    case (diffY, diffX) => if papers.isDefinedAt(row + diffY) && papers(row+diffY).isDefinedAt(col + diffX) then papers(row+diffY)(col+diffX) else false
  .count(_ == true) < 4