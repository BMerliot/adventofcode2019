import scala.io.Source

object Day01 {
    def main(args: Array[String]) {

        def computeFuel(n: Int): List[Int] = {
            val m = n / 3 - 2;
            if (m < 0) List[Int]()
            else m :: computeFuel(m)
        }

        val source = Source.fromFile("src/main/resources/day01.txt")
        val res = source.getLines
          .flatMap( (line: String) => computeFuel(line.toInt) )
          .sum
        source.close()

        println(res)
    }
}
