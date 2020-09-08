package days

import scala.io.Source

object Day03 {
  def main(args: Array[String]) {

    def getPath(l: List[String]): List[(Int, Int)] = {

      def getPathRec(l: List[String], c: (Int, Int)): List[(Int, Int)] =
        l match {
          case Nil => c :: List.empty
          case h :: t => h match {
            case h if h.startsWith("R") =>
              val upperBound = c._2 + h.takeRight(h.length - 1).toInt
              (c._2 until upperBound)
                .map((x: Int) => (c._1, x))
                .toList ::: getPathRec(t, (c._1, upperBound))
            case h if h.startsWith("U") =>
              val upperBound = c._1 + h.takeRight(h.length - 1).toInt
              (c._1 until upperBound)
                .map((x: Int) => (x, c._2))
                .toList ::: getPathRec(t, (upperBound, c._2))
            case h if h.startsWith("L") =>
              val lowerBound = c._2 - h.takeRight(h.length - 1).toInt
              (c._2 until lowerBound by -1)
                .map((x: Int) => (c._1, x))
                .toList ::: getPathRec(t, (c._1, lowerBound))
            case h if h.startsWith("D") =>
              val lowerBound = c._1 - h.takeRight(h.length - 1).toInt
              (c._1 until lowerBound by -1)
                .map((x: Int) => (x, c._2))
                .toList ::: getPathRec(t, (lowerBound, c._2))
            case h => throw new Exception("Expected R, U, L or D, got " + h)
          }
        }

      getPathRec(l, (0, 0))
    }

    val source = Source.fromFile("src/main/resources/day03.txt")
    val wires: List[List[String]] = source.getLines
        .map((s: String) => s.split(",").toList)
        .toList

    val paths: List[List[(Int, Int)]] = wires
      .map((l: List[String]) => getPath(l))

    val intersections: Set[(Int, Int)] = paths.head.toSet.intersect(paths(1).toSet)
      .diff(Set((0, 0))) // Removing (0, 0) since it is indeed the closest intersection to (0, 0)

    val part1: Int = intersections
      .foldLeft(Int.MaxValue)((dista: Int, b: (Int, Int)) => {
        val distb: Int = Math.abs(b._1) + Math.abs(b._2)
        if (dista < distb) dista else distb
      })

    val part2: Int = intersections
      .toList
      .map((t: (Int, Int)) => paths.head.indexOf(t) + paths(1).indexOf(t))
      .min

    source.close()

    println(part1)
    println(part2)
  }
}
