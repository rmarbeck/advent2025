import lib.MachineSolver

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.*

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val machines =
      inputLines.map: line =>
        val (lightsO, boutons, joltageO) = line.split(" ").foldLeft((Option.empty[Lights], Seq.empty[Bouton], Option.empty[Joltage])):
          case ((_, boutonsA, joltageA), LightsExt(machine)) => (Some(machine), boutonsA, joltageA)
          case ((lightsA, boutonsA, joltageA), BoutonExt(bouton)) => (lightsA, bouton +: boutonsA, joltageA)
          case ((lightsA, boutonsA, _), JoltageExt(joltage)) => (lightsA, boutonsA, Some(joltage))
          case (acc, _) => acc

        (lightsO, joltageO) match
          case (Some(lights), Some(joltage)) => (lights, boutons, joltage)
          case _ => throw Exception("not supported")

    val result1 =
      machines.map:
        case (machine, boutons, _) => loop(machine.target, boutons)
      .sum

    import scala.collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
    val result2 = machines.par.map:
      case (_, boutons, joltage) =>
        val resultO =  MachineSolver.solveMachine(joltage.toArray, toArray(boutons, joltage.values.length)).map(_.sum)
        resultO.getOrElse(-1L)
    .sum

    (result1.toString, result2.toString)

end Solution

@tailrec
def loop(target: Int, boutons: Seq[Bouton], values: mutable.BitSet = mutable.BitSet(0), iteration: Int = 0): Int =
  if values.contains(target) then
    iteration
  else
    val results = values.toSeq.flatMap: value =>
      boutons.map(_.xor(value))
    loop(target, boutons, mutable.BitSet(results:_*), iteration + 1)

def toArray(boutons: Seq[Bouton], size: Int): Array[Array[Int]] =
  boutons.toArray.map:
    _.toArray(size)

case class Lights(target: Int)
case class Bouton(values: Seq[Int]):
  val internalValue: Int = values.map(math.pow(2, _)).sum.toInt
  val asBitSet: mutable.BitSet = mutable.BitSet(values:_*)
  def xor(other: Int): Int = internalValue ^ other
  def toArray(size: Int): Array[Int] =
    val builder = Array.fill[Int](size)(0)
    values.foreach(index => builder(index) = 1)
    builder

case class Joltage(values: Seq[Int]):
  def toArray: Array[Int] =
    values.toArray

object JoltageExt:
  def unapply(str: String): Option[Joltage] =
    str match
      case s"{$values}" => Some(Joltage(values.split(',').toSeq.map(_.toInt)))
      case _ => None

object BoutonExt:
  def unapply(str: String): Option[Bouton] =
    str match
      case s"($values)" => Some(Bouton(values.split(',').toSeq.map(_.toInt)))
      case _ => None

object LightsExt:
  def unapply(str: String): Option[Lights] =
    str match
      case s"[$values]" =>
        val finalValue = values.reverse.foldLeft(0):
          case (acc, '.') => acc * 2
          case (acc, '#') => acc * 2 + 1
        Some(Lights(finalValue))
      case _ => None