package days

import scala.annotation.tailrec

object Day04 {

  def main(args: Array[String]): Unit = {

    def isValid1(n: Int): Boolean = {

      @tailrec
      def isValidRec(n: Int, doubleEncountered: Boolean): Boolean = {
        val a = n / 10
        val b = n % 10
        a match {
          case 0 => doubleEncountered
          case _ =>
            val c = a % 10
            if (c == b) isValidRec(a, doubleEncountered = true)
            else (c < b) && isValidRec(a, doubleEncountered)
        }
      }

      isValidRec(n, doubleEncountered = false)
    }

    val part1 = (264360 to 746325).count((n: Int) => isValid1(n))
    println(part1)

    // Part 2 could be achieved the same way as part 1, with a look over of 3
    // and some new arguments in isValidRec. But let's try something different.
    def isValid2(n: Int): Boolean = {

      val occurrences = n.toString.foldLeft(List[(Char, Int)]())(
        (l: List[(Char, Int)], c: Char) => {
          l match {
            case (d, i) :: t if (c == d) => (c, i + 1) :: t
            case _ => (c, 1) :: l
          }
        }
      )

      occurrences.exists(_._2 == 2) &&
        occurrences.reduceLeft(
          (a: (Char, Int), b: (Char, Int)) => (b._1, if (a._1 > b._1) a._2 else 0)
        )._2 > 0
    }

    val part2 = (264360 to 746325).count((n: Int) => isValid2(n))
    println(part2)
  }
}
