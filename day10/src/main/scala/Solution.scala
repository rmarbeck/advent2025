import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.*

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val machines =
      inputLines.map: line =>
        val (machineO, boutons, voltageO) = line.split(" ").foldLeft((Option.empty[Machine], Seq.empty[Bouton], Option.empty[Voltage])):
          case ((machineA, boutonsA, voltageA), MachineExt(machine)) => (Some(machine), boutonsA, voltageA)
          case ((machineA, boutonsA, voltageA), BoutonExt(bouton)) => (machineA, bouton +: boutonsA, voltageA)
          case ((machineA, boutonsA, voltageA), VoltageExt(voltage)) => (machineA, boutonsA, Some(voltage))
          case (acc, _) => acc

        (machineO, voltageO) match
          case (Some(machine), Some(voltage)) => (machine, boutons, voltage)
          case _ => throw Exception("not supported")


    val result1 =
      machines.map:
        case (machine, boutons, _) => loop(machine.target, boutons)
      .sum

    import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
    val result2 = machines.par.map:
      case (_, boutons, voltage) =>
        val resultO =  loopPart2c(boutons, voltage)
        println(resultO)
        resultO.getOrElse(-1)
    .sum

    (result1.toString, result2.toString)

end Solution

def toArray(boutons: Seq[Bouton], size: Int): Array[Array[Double]] =
  boutons.toArray.map:
    _.toArray(size)

def loopPart2c(
                boutons: Seq[Bouton],
                target: Voltage,
                ignoredCost: Int = 0,
                ignoredBestCostO: Option[Int] = None
              ): Option[Int] =
  // On passe tout en structures bas niveau : Array[Array[Int]] + Array[Int]
  val bs: Array[Array[Int]] =
    boutons.map(_.values.toArray).toArray // indices affectés par chaque bouton

  val nButtons = bs.length
  val baseRest: Array[Int] = target.values.toArray
  val dim = baseRest.length

  // rest : vecteur résiduel modifiable (on y enlève les appuis)
  val rest: Array[Int] = Array.ofDim[Int](dim)

  // DFS sur les boutons, avec un budget d'appuis restant
  def dfs(idx: Int, remaining: Int): Boolean =
    // Plus de budget → impossible
    if remaining < 0 then
      false
    // Plus de boutons → solution seulement si plus rien à faire et tout est à 0
    else if idx == nButtons then
      if remaining != 0 then
        false
      else
        var i = 0
        while i < dim do
          if rest(i) != 0 then return false
          i += 1
        true
    else
      val btnIndices = bs(idx)
      val len = btnIndices.length

      // Cas trivial : bouton qui ne touche rien
      if len == 0 then
        // On ne peut que le presser 0 fois, donc on passe au suivant
        return dfs(idx + 1, remaining)

      // Calcul du k max possible pour ce bouton :
      //  - ne pas dépasser remaining
      //  - ne pas rendre négatif une composante de rest
      var maxK = remaining
      var j = 0
      while j < len do
        val pos = btnIndices(j)
        val v = rest(pos)
        if v < maxK then maxK = v
        j += 1
      if maxK < 0 then
        return false

      // Essayer k = 0..maxK
      var k = 0
      while k <= maxK do
        // Appliquer k appuis : rest[pos] -= k pour chaque index du bouton
        if k > 0 then
          j = 0
          while j < len do
            val pos = btnIndices(j)
            rest(pos) -= k
            j += 1

        // Descendre
        if dfs(idx + 1, remaining - k) then
          return true

        // Undo : remonter rest
        if k > 0 then
          j = 0
          while j < len do
            val pos = btnIndices(j)
            rest(pos) += k
            j += 1

        k += 1

      false

  // upper bound sur le budget :
  // on ne pourra jamais faire plus d'appuis que la somme des composantes
  var sum = 0
  var i = 0
  while i < dim do
    sum += baseRest(i)
    i += 1
  val maxBudget = sum

  // Iterative deepening sur le budget : on teste budget = 0,1,2,...
  var b = 0
  while b <= maxBudget do
    // réinitialiser rest = baseRest
    System.arraycopy(baseRest, 0, rest, 0, dim)

    if dfs(0, b) then
      return Some(b)

    b += 1

  None


def loopPart2b(boutons: Seq[Bouton], target: Voltage, cost: Int = 0, bestCostO: Option[Int] = None): Option[Int] =
  //println(s"call ${boutons}, ${target} <-> ${cost}, ${bestCostO}")
  if bestCostO.exists(_ <= cost) then
    return None

  if boutons.isEmpty then
    if target == target.solved then
      Some(cost)
    else None
  else
    val current = boutons.head
    val maxUsefulPresses = bestCostO match
      case Some(best) => math.max(0, best - cost) // au-delà, cost+current >= best
      case None => Int.MaxValue // pas encore de borne
    explorePresses(untilImpossible2(current, target, maxUsefulPresses).toList, bestCostO, cost, boutons)


@tailrec
private def explorePresses(
                            remaining: List[(Voltage, Int)],       // (newTarget, addedCost)
                            bestCostO: Option[Int],
                            cost: Int,
                            boutons: Seq[Bouton]
                          ): Option[Int] =
  (remaining, bestCostO) match

    // Plus rien à tester
    case (Nil, _) =>
      bestCostO

    // Cas "early stop global" si tes addedCost sont triés par ordre croissant :
    // si le premier restant est déjà trop cher, tous les suivants le seront aussi
    case ((_, addedCost) :: _, Some(bestCost)) if bestCost <= cost + addedCost =>
      bestCostO

    // Cas normal : on teste ce candidat
    case ((newTarget, addedCost) :: tail, _) =>
      val newCost = cost + addedCost

      // On explore la suite avec ce bouton
      val candidateO = loopPart2b(boutons.tail, newTarget, newCost, bestCostO)

      // On met à jour le best
      val newBest =
        if candidateO.exists(_ < bestCostO.getOrElse(Int.MaxValue)) then
          candidateO
        else
          bestCostO

      // On continue avec le reste
      explorePresses(tail, newBest, cost, boutons)


def untilImpossible2(bouton: Bouton, target: Voltage, maxPresses: Int, current: Int = 0): Seq[(Voltage, Int)] =
  if current > maxPresses then
    Seq()
  else
    val newRest = target.plus(bouton, -current)
    //println(s"ui2 $current -> ${target} -<- ${newRest}")
    if newRest.isValid then
      (newRest, current) +: untilImpossible2(bouton, target, maxPresses, current + 1)
    else
      Seq()


def loopPart2(boutons: Seq[Bouton], target: Voltage, partials: Set[(Voltage, Int)], bestO: Option[Int] = None): Int = {
  //println(s"in loop part2 ${boutons.length}")
  if boutons.isEmpty || partials.isEmpty then
    bestO.getOrElse(-1)
  else
    val current = boutons.head
    val updatedPartials =  untilImpossible(current, target, partials)
    val newBestO = updatedPartials.filter(_._1 == target).map(_._2).minOption
    val nextBestO = (newBestO, bestO) match
      case (Some(newBest), Some(best)) => Some(math.min(newBest, best))
      case (None, Some(best)) => bestO
      case (Some(newBest), None) => newBestO
      case _ => None

    val nextPartials = nextBestO.map: nextBest =>
      updatedPartials.filter(_._2 <= nextBest)
    .getOrElse(updatedPartials)

    loopPart2(boutons.tail, target, nextPartials, nextBestO)
}

@tailrec
def untilImpossible(bouton: Bouton, target: Voltage, partials: Set[(Voltage, Int)], current: Int = 0, updatedPartials: Set[(Voltage, Int)] =  Set.empty[(Voltage, Int)]): Set[(Voltage, Int)] =
  val newPartials = partials.flatMap:
    case (voltage, cost) =>
      val updatedVoltage = voltage.plus(bouton, current)
      if target.isValidForTarget(updatedVoltage) then
        Some((updatedVoltage, cost + current))
      else
        None

  if newPartials.isEmpty then
    updatedPartials
  else
    untilImpossible(bouton, target, partials, current + 1, updatedPartials ++ newPartials)


@tailrec
def loop(target: Int, boutons: Seq[Bouton], values: mutable.BitSet = mutable.BitSet(0), iteration: Int = 0): Int =
  if values.contains(target) then
    iteration
  else
    val results = values.toSeq.flatMap: value =>
      boutons.map(_.xor(value))
    loop(target, boutons, mutable.BitSet(results:_*), iteration + 1)

case class Machine(target: Int)
case class Bouton(values: Seq[Int]):
  val internalValue: Int = values.map(math.pow(2, _)).sum.toInt
  val asBitSet: mutable.BitSet = mutable.BitSet(values:_*)
  def contains(index: Int): Boolean = asBitSet.contains(index)
  def xor(other: Int): Int = internalValue ^ other
  def toArray(size: Int): Array[Double] =
    val builder = Array.fill[Double](size)(0.0d)
    values.foreach(index => builder(index) = 1d)
    builder



  override def toString: String = values.mkString("(", ",", ")")

case class Voltage(values: Seq[Int]):
  lazy val start, solved: Voltage = Voltage(values.map(_ => 0))
  def plus(bouton: Bouton, nTimes: Int): Voltage =
    val updatedValues = values.zipWithIndex.map:
      case (current, index) if bouton.contains(index) => current + nTimes
      case (current, _) => current
    Voltage(updatedValues)


  def isValid: Boolean =
    this.values.forall(_ >= 0)

  def isValidForTarget(toTest: Voltage): Boolean =
    toTest.values.zip(this.values).forall((test, target) => test <= target)

  def toArray: Array[Double] =
    values.map(_.toDouble).toArray

object VoltageExt:
  def unapply(str: String): Option[Voltage] =
    str match
      case s"{$values}" => Some(Voltage(values.split(',').toSeq.map(_.toInt)))
      case _ => None

object BoutonExt:
  def unapply(str: String): Option[Bouton] =
    str match
      case s"($values)" => Some(Bouton(values.split(',').toSeq.map(_.toInt)))
      case _ => None

object MachineExt:
  def unapply(str: String): Option[Machine] =
    str match
      case s"[$values]" =>
        val finalValue = values.reverse.foldLeft(0):
          case (acc, '.') => acc * 2
          case (acc, '#') => acc * 2 + 1
        Some(Machine(finalValue))
      case _ => None