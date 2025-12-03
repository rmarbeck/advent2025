import scala.collection.mutable

type BothResults = (Long, Long)

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    given cache: mutable.Map[Int, Set[Int]] = mutable.Map()

    import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
    val (result1, result2) = inputLines.head.split(",").par.collect:
      case s"$start-$end" =>
        (start.toLong to end.toLong).foldLeft((0L, 0L)):
          case (acc, index) if index.isInvalidPart1And2 => acc + (index, index)
          case (acc, index) if index.isInvalidPart2Only => acc + (0, index)
          case (acc, _)                                 => acc
    .reduce(_ + _)

    (result1.toString, result2.toString)

end Solution

extension (tuple: BothResults)
  def +(other: BothResults): BothResults = (tuple._1 + other._1, tuple._2 + other._2)

extension (index: Long)
  def isInvalidPart1And2: Boolean =
    val asStr = index.toString
    asStr.length match
      case value if value % 2 == 0 =>
        val (first, last) = asStr.splitAt(value / 2)
        first == last
      case _  => false

  def isInvalidPart2Only(using cache: mutable.Map[Int, Set[Int]]): Boolean =
    val asStr = index.toString
    def splitMatch(partsSize: Int): Boolean =
      val groups = asStr.grouped(asStr.length / partsSize).toSeq
      groups.tail.forall(_ == groups.head)

    divisorsCached(asStr.length).exists(splitMatch)

def divisorsCached(until: Int)(using cache: mutable.Map[Int, Set[Int]]): Set[Int] =
  cache.getOrElseUpdate(until, divisors(until))

def divisors(until: Int): Set[Int] = primesBut2(until).filter(until % _ == 0)

def primesBut2(until: Int): Set[Int] = (3 to until).filter(isPrime).toSet

def isPrime(n: Int): Boolean =
  List.range(2, n) forall (x => n % x != 0)