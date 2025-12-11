import scala.collection.immutable.BitSet
import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val nextDevices = inputLines.foldLeft(Map.empty[Device, Seq[Device]]):
      case (acc, s"$src: $destinations") =>
        val dest = destinations.split(" ").map(Device.apply).toSeq
        acc + (Device(src) -> dest)
      case _ => throw Exception("not supported")

    given nextDevicesWithOut: Map[Device, Seq[Device]] = nextDevices + (Device("out") -> Seq.empty[Device])

    val firstPath = countPaths(Device("svr"), Device("fft"))
    val secondPath = countPaths(Device("fft"), Device("dac"))
    val thirdPath = countPaths(Device("dac"), Device("out"))

    val result1 = countPaths(Device("svr"), Device("out"))
    val result2 = firstPath * secondPath * thirdPath

    (result1.toString, result2.toString)

end Solution

def countPaths( start: Device,
                target: Device)(using nextDevices: Map[Device, Seq[Device]]): Long =
  val memo = collection.mutable.Map.empty[Device, Long]

  def loop(d: Device): Long =
    memo.getOrElseUpdate(d,
      if d == target then 1L
      else nextDevices(d).map(loop).sum
    )

  loop(start)

case class Device(name: String)
