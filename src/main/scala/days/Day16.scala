package days

import scala.io.Source

object Day16 {

  def fft(a: Array[Int]): Array[Int] = {

    a.zipWithIndex.map{
      case (_, step) => math.abs(
        a.drop(step).zipWithIndex.filter{
          case (_, i) => (1 + i / (step + 1)) % 2 == 1
        }.map{
          case (x, i) => (1 + i / (step + 1)) % 4 match {
            case 1 => x
            case 3 => - x
          }
        }.sum
      ) % 10
    }
  }

  def main(args: Array[String]): Unit = {

    val source = Source.fromFile("src/main/resources/day16.txt")
    val part1Init: Array[Int] = source.getLines().next()
        .map(_.toInt - '0').toArray
    source.close()

    lazy val phraseStream1: LazyList[Array[Int]] =
      fft(part1Init) #::
        phraseStream1.map(fft)
    val part1: String = phraseStream1
      .take(100).last
      .take(8)
      .mkString("")

    println(part1)

    // Thanks again reddit for the trick
    // the lower half of the array is multiplied by a upper triangular matrix of ones at each fft step
    // which is equivatent to a partial sum of the following digits
    val part2Init: Array[Int] =
      Array.fill(10000)(part1Init).flatten
    val offSet: Int = part1Init.take(7).foldLeft(0)(_ * 10 + _)
    println(offSet)
    val part2: String = (1 to 100).foldLeft(part2Init.drop(offSet).reverse){
      (a, _) => a.scanLeft(0)(_+_).map(_ % 10)
    }
      .reverse
      .take(8)
      .mkString

    println(part2)
  }
}
