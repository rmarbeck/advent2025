object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val firstPositionOfSafe = 50

    val safeHistory = inputLines.iterator.foldLeft(SafeHistory(firstPositionOfSafe)):
      case (sh, MoveExtractor(move)) => sh.next(move)


    val result1 = s"${safeHistory.zeroesStops}"
    val result2 = s"${safeHistory.zeroesPassedThrough}"

    (result1.toString, result2.toString)

  case class SafeHistory(currentPosition: Int, zeroesStops: Int = 0, zeroesPassedThrough: Int = 0):
    def next(move: Move): SafeHistory =
      val rawPosition = move.next(currentPosition)
      val zeroes = Math.floorDiv(rawPosition - currentPosition, 100).abs
      val signChange = if zeroes == 0 && (currentPosition.sign - rawPosition.sign).abs == 2 && rawPosition != 0 then 1 else 0
      val finalPosition = rawPosition % 100
      if finalPosition == 0 then
        this.copy(currentPosition = finalPosition, zeroesStops = zeroesStops + 1, zeroesPassedThrough = zeroesPassedThrough + zeroes + signChange)
      else
        this.copy(currentPosition = finalPosition, zeroesPassedThrough = zeroesPassedThrough + zeroes)


  sealed trait Move:
    val steps: Int
    def withDirection: Int
    def next(position: Int): Int = position + withDirection
  private case class Left(steps: Int) extends Move:
    override def withDirection: Int = -steps
  private case class Right(steps: Int) extends Move:
    override def withDirection: Int = steps

  private object MoveExtractor:
    def unapply(str: String): Option[Move] =
      str match
        case s"L${clicks}" => clicks.toIntOption.map(Left(_))
        case s"R${clicks}" => clicks.toIntOption.map(Right(_))
        case _ => None

end Solution

