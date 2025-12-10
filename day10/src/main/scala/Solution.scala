object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val result =
      inputLines.map: line =>
        val (machineO, boutons, voltages) = line.split(" ").foldLeft((Option.empty[Machine], Seq.empty[Bouton], Seq.empty[Voltage])):
          case ((machineA, boutonsA, voltagesA), MachineExt(machine)) => (Some(machine), boutonsA, voltagesA)
          case ((machineA, boutonsA, voltagesA), BoutonExt(bouton)) => (machineA, bouton +: boutonsA, voltagesA)
          case ((machineA, boutonsA, voltagesA), VoltageExt(voltage)) => (machineA, boutonsA, voltage +: voltagesA)
          case (acc, _) => acc

        machineO match
          case Some(machine) => (machine, boutons, voltages)
          case None => throw Exception("not supported")


    //
    // Code is here
    //



    val result1 = s""
    val result2 = s""

    (result1.toString, result2.toString)

end Solution

case class Machine(target: Int)
case class Bouton(values: Seq[Int])
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
        val finalValue = values.foldLeft(0):
          case (acc, '.') => acc * 2
          case (acc, '#') => acc * 2 + 1
        Some(Machine(finalValue))
      case _ => None