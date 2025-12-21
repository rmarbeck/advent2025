import scala.collection.mutable

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val reds = inputLines.map: line =>
      val Array(xs, ys) = line.split(",")
      Pt(xs.toInt, ys.toInt)
    .toIndexedSeq

    val n = reds.length
    if n < 2 then
      return ("0", "0")

    // -------------------------
    // Part 1 : plus grand rectangle avec 2 rouges aux coins
    // -------------------------

    var bestPart1 = 0L
    var i = 0
    while i < n do
      val a = reds(i)
      var j = i + 1
      while j < n do
        val b = reds(j)
        if a.x != b.x && a.y != b.y then
          val area =
            math.abs(a.x - b.x) * math.abs(a.y - b.y) // Long * Long => Long
          if area > bestPart1 then bestPart1 = area
        j += 1
      i += 1

    // -------------------------
    // Coord compression pour la Part 2
    // -------------------------

    val xsSet = mutable.HashSet[Long]()
    val ysSet = mutable.HashSet[Long]()

    // On met pour chaque tuile : x, x+1 et y, y+1
    reds.foreach: p =>
      xsSet += p.x
      xsSet += (p.x + 1)
      ysSet += p.y
      ysSet += (p.y + 1)

    val xs = xsSet.toArray
    java.util.Arrays.sort(xs)
    val ys = ysSet.toArray
    java.util.Arrays.sort(ys)

    val Wc = xs.length - 1 // nb de colonnes compressées
    val Hc = ys.length - 1 // nb de lignes compressées

    val xIndex = mutable.HashMap[Long, Int]()
    i = 0
    while i < xs.length do
      xIndex(xs(i)) = i
      i += 1

    val yIndex = mutable.HashMap[Long, Int]()
    i = 0
    while i < ys.length do
      yIndex(ys(i)) = i
      i += 1

    // allowed(j)(i) : cellule compressée (ligne j, colonne i)
    val allowed = Array.ofDim[Boolean](Hc, Wc)

    // -------------------------
    // Marquage de la boucle (rouge + vert) sur la grille compressée
    // -------------------------

    def markHorizontal(y: Long, x1: Long, x2: Long): Unit =
      val iy = yIndex(y) // bord bas de la bande
      val ix1 = xIndex(x1)
      val ix2 = xIndex(x2 + 1) // exclusif
      var ix = ix1
      while ix < ix2 do
        if iy >= 0 && iy < Hc && ix >= 0 && ix < Wc then
          allowed(iy)(ix) = true
        ix += 1

    def markVertical(x: Long, y1: Long, y2: Long): Unit =
      val ix = xIndex(x)
      val iy1 = yIndex(y1)
      val iy2 = yIndex(y2 + 1) // exclusif
      var iy = iy1
      while iy < iy2 do
        if iy >= 0 && iy < Hc && ix >= 0 && ix < Wc then
          allowed(iy)(ix) = true
        iy += 1

    var k = 0
    while k < n do
      val a = reds(k)
      val b = reds((k + 1) % n)
      if a.x == b.x then
        // segment vertical
        val x = a.x
        val y1 = math.min(a.y, b.y)
        val y2 = math.max(a.y, b.y)
        markVertical(x, y1, y2)
      else if a.y == b.y then
        // segment horizontal
        val y = a.y
        val x1 = math.min(a.x, b.x)
        val x2 = math.max(a.x, b.x)
        markHorizontal(y, x1, x2)
      else
        // ne devrait pas arriver d'après l'énoncé
        sys.error(s"Non axis-aligned segment: $a -> $b")
      k += 1

    // -------------------------
    // Flood fill extérieur sur la grille compressée étendue
    // -------------------------

    val visited = Array.ofDim[Boolean](Hc + 2, Wc + 2)
    val q = mutable.Queue[(Int, Int)]()

    def isBlocked(ey: Int, ex: Int): Boolean =
      // ey/ex : indices sur la grille étendue [0..Hc+1] x [0..Wc+1]
      if ey == 0 || ex == 0 || ey == Hc + 1 || ex == Wc + 1 then
        false // bord extérieur jamais bloqué
      else
        val jy = ey - 1
        val ix = ex - 1
        allowed(jy)(ix) // les cellules de la boucle sont des murs

    visited(0)(0) = true
    q.enqueue((0, 0))

    val dirs = Array((1, 0), (-1, 0), (0, 1), (0, -1))

    while q.nonEmpty do
      val (cy, cx) = q.dequeue()
      var d = 0
      while d < 4 do
        val (dy, dx) = dirs(d)
        val ny = cy + dy
        val nx = cx + dx
        if ny >= 0 && ny <= Hc + 1 && nx >= 0 && nx <= Wc + 1 then
          if !visited(ny)(nx) && !isBlocked(ny, nx) then
            visited(ny)(nx) = true
            q.enqueue((ny, nx))
        d += 1

    // Toutes les cases étendues (ey,ex) non visitées et non murs sont à l'intérieur
    var jy = 0
    while jy < Hc do
      var ix = 0
      while ix < Wc do
        val ey = jy + 1
        val ex = ix + 1
        if !visited(ey)(ex) then
          // intérieur : on le marque aussi comme autorisé (vert)
          allowed(jy)(ix) = true
        ix += 1
      jy += 1

    // -------------------------
    // Préfixe 2D pondéré (en nombre réel de tuiles)
    // -------------------------

    val ps = Array.ofDim[Long](Hc + 1, Wc + 1)

    jy = 0
    while jy < Hc do
      var rowSum = 0L
      val dy = (ys(jy + 1) - ys(jy)).toLong
      var ix = 0
      while ix < Wc do
        val dx = (xs(ix + 1) - xs(ix)).toLong
        val cellArea = dx * dy
        if allowed(jy)(ix) then
          rowSum += cellArea
        ps(jy + 1)(ix + 1) = ps(jy)(ix + 1) + rowSum
        ix += 1
      jy += 1

    def sumRect(i1: Int, j1: Int, i2: Int, j2: Int): Long =
      // indices inclusifs sur la grille compressée
      if i1 > i2 || j1 > j2 then 0L
      else
        val a = ps(j2 + 1)(i2 + 1)
        val b = ps(j1)(i2 + 1)
        val c = ps(j2 + 1)(i1)
        val d = ps(j1)(i1)
        a - b - c + d

    // -------------------------
    // Part 2 : max rectangle 100 % dans rouge+vert
    // -------------------------

    val redsArr = reds.toArray
    var bestPart2 = 0L

    i = 0
    while i < n do
      val a = redsArr(i)
      var j = i + 1
      while j < n do
        val b = redsArr(j)
        if a.x != b.x && a.y != b.y then
          val xl = math.min(a.x, b.x)
          val xr = math.max(a.x, b.x)
          val yb = math.min(a.y, b.y)
          val yt = math.max(a.y, b.y)

          val width = xr - xl + 1L
          val height = yt - yb + 1L
          val rectArea = width * height

          if rectArea > bestPart2 then
            // On mappe les bords du rectangle sur les indices compressés
            val i1 = xIndex(xl)
            val i2e = xIndex(xr + 1) // exclusif
            val j1 = yIndex(yb)
            val j2e = yIndex(yt + 1) // exclusif

            val areaInside = sumRect(i1, j1, i2e - 1, j2e - 1)

            if areaInside == rectArea then
              bestPart2 = rectArea

        j += 1
      i += 1


    val result1 = bestPart1
    val result2 = bestPart2

    (result1.toString, result2.toString)

end Solution

case class Pt(x: Int, y: Int)