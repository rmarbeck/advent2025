import annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =
    val (countDown, limit) = inputLines.length match
      case 20 => (10, 10)
      case _ => (1000, 1000)

    val junctionBoxes = inputLines.collect:
      case JunctionBoxExt(jb) => jb

    val jbDistancesSorted = junctionBoxes.combinations(2).collect:
      case Seq(jb1, jb2) => ((jb1, jb2), jb1.distance(jb2))
    .toList.sortBy(_._2).map(_._1)

    val (result1, result2) = connectJunctions(jbDistancesSorted, countDown, limit)

    (result1.toString, result2.toString)

end Solution

type Boxes = (JunctionBox, JunctionBox)

@tailrec
def connectJunctions(closestBoxes: Seq[Boxes], countDown: Int, limit: Int, best3CircuitsProduct: Option[Int] = None, lastJunction: Option[Boxes] = None, circuits: Seq[Set[JunctionBox]] = Seq.empty[Set[JunctionBox]], junctions: Set[JunctionBox] = Set.empty[JunctionBox]): (Int, Long) =
  if countDown == 0 then
    connectJunctions(closestBoxes, countDown - 1, limit, Some(countPart1(circuits)), lastJunction, circuits, junctions)
  else if junctions.size == limit || closestBoxes.isEmpty then
    (best3CircuitsProduct.getOrElse(0), lastJunction.map(countPart2).getOrElse(0L))
  else
    val current = closestBoxes.head

    val (withCurrentJunction, withoutCurrentJunction) = circuits.partition(circuit => circuit.contains(current._1) || circuit.contains(current._2))
    if withCurrentJunction.exists(circuit => circuit.contains(current._1) && circuit.contains(current._2)) then
      connectJunctions(closestBoxes.tail, countDown - 1, limit, best3CircuitsProduct, Some(current), circuits, junctions)
    else
      val mergedWithHead = withCurrentJunction.find(_.contains(current._1)).getOrElse(Set(current._1))
      val mergedWithLast = withCurrentJunction.find(_.contains(current._2)).getOrElse(Set(current._2))
      val merged = mergedWithLast ++ mergedWithHead
      connectJunctions(closestBoxes.tail, countDown - 1, limit, best3CircuitsProduct, Some(current), merged +: withoutCurrentJunction, junctions + current._1 + current._2)

def countPart1(circuits: Seq[Set[JunctionBox]]): Int = circuits.map(_.size).sortBy(-_).take(3).product
def countPart2(boxes: Boxes): Long = boxes._1.x.toLong * boxes._2.x.toLong

case class JunctionBox(x: Int, y: Int, z: Int):
  def distance(other: JunctionBox): Double =
    math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2) + math.pow(z - other.z, 2))

object JunctionBoxExt:
  def unapply(str: String): Option[JunctionBox] =
    str match
      case s"$x,$y,$z" => Some(JunctionBox(x.toInt, y.toInt, z.toInt))
      case _ => None