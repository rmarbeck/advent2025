import scala.annotation.tailrec
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val machines =
      inputLines.map: line =>
        val (machineO, boutons, voltages) = line.split(" ").foldLeft((Option.empty[Machine], Seq.empty[Bouton], Seq.empty[Voltage])):
          case ((machineA, boutonsA, voltagesA), MachineExt(machine)) => (Some(machine), boutonsA, voltagesA)
          case ((machineA, boutonsA, voltagesA), BoutonExt(bouton)) => (machineA, bouton +: boutonsA, voltagesA)
          case ((machineA, boutonsA, voltagesA), VoltageExt(voltage)) => (machineA, boutonsA, voltage +: voltagesA)
          case (acc, _) => acc

        machineO match
          case Some(machine) => (machine, boutons, voltages)
          case None => throw Exception("not supported")


    val result1 =
      machines.map:
        case (machine, boutons, _) => loop(machine.target, boutons)
      .sum


    val result2 = s""

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

case class Machine(target: Int)
case class Bouton(values: Seq[Int]):
  val internalValue: Int = values.map(math.pow(2, _)).sum.toInt
  def xor(other: Int): Int = internalValue ^ other

  override def toString: String = super.toString + s" => ($internalValue)"

case class Voltage(values: Seq[Int])

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