object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val junctionBoxes = inputLines.collect:
      case JunctionBoxExt(jb) => jb

    val jbDistancesSorted = junctionBoxes.combinations(2).collect:
      case Seq(jb1, jb2) => ((jb1, jb2), jb1.distance(jb2))
    .toList.sortBy(_._2).map(_._1)

    val result1 = part1(jbDistancesSorted, 10)
    val result2 = s""

    (result1.toString, result2.toString)

end Solution

def part1(jbDistances: List[(JunctionBox, JunctionBox)], remaining: Int, circuits: Seq[Seq[JunctionBox]] = Seq.empty[Seq[JunctionBox]]): Int =
  if remaining == 0 then
    circuits.map(_.length).product
  else
    val current = jbDistances.head
    val (withCurrentJunction, withoutCurrentJunction)  = circuits.partition(circuit => circuit.contains(current._1) || circuit.contains(current._2))
    if withCurrentJunction.isEmpty then
      part1(jbDistances.tail, remaining - 1, withoutCurrentJunction)
    else
      if withCurrentJunction.exists(circuit => circuit.contains(current._1) && circuit.contains(current._2)) then
        part1(jbDistances.tail, remaining - 1, circuits)
      else
        ???


case class JunctionBox(x: Int, y: Int, z: Int):
  def distance(other: JunctionBox): Double =
    Math.sqrt((x - other.x) ^ 2 + (y - other.y) ^ 2 + (z - other.z) ^ 2)

object JunctionBoxExt:
  def unapply(str: String): Option[JunctionBox] =
    str match
      case s"$x,$y,$z" => Some(JunctionBox(x.toInt, y.toInt, z.toInt))
      case _ => None