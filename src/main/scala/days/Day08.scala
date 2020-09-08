package days

import scala.io.Source

object Day08 {
  def main(args: Array[String]) {

    val source = Source.fromFile("src/main/resources/day08.txt")
    val layers: List[List[Int]] = source.getLines
      .next()
      .map((c: Char) => c.asDigit)
      .grouped(25*6)
      .map(_.toList)
      .toList

    source.close()

    val part1: Map[Int, Int] = layers
      .map(
        _.groupBy(identity)
        .view.mapValues(_.size).toMap
      ).minBy(_.getOrElse(0, 0))

    println(part1.getOrElse(1, 0) * part1.getOrElse(2, 0))

    val part2: List[String] = layers
      .reduceLeft((a: List[Int], b: List[Int]) =>
        a.zip(b).map((x: (Int, Int)) => if (x._1 == 2) x._2 else x._1)
      ).map {
      case 0 => "  "
      case 1 => "XX"
      case 2 => "EE"
    }.grouped(25).toList
      .map(_.mkString(""))

    part2.foreach(println)
  }
}
