import scala.annotation.tailrec
import scala.io.Source
import scala.runtime.ScalaRunTime.stringOf

object Day02 {
  def main(args: Array[String]) {

    @tailrec
    def runProgram(i: Int, a: Array[Int]): Array[Int] = {
      def read4(i: Int): Boolean = {
        a(i) match {
          case 1 => a(a(i+3)) = a(a(i+1)) + a(a(i+2)); false
          case 2 => a(a(i+3)) = a(a(i+1)) * a(a(i+2)); false
          case 99 => true
          case _ => throw new Exception("Should read instruction 1, 2 or 99")
        }
      }

      if (read4(i)) a
      else runProgram(i+4, a)
    }

    val source = Source.fromFile("src/main/resources/day02.txt")
    val inputArray = source.getLines
      .find(_ => true)
      .getOrElse("99")
      .split(",")
      .map((s: String) => s.toInt)

    val part1: Array[Int] = runProgram(0, inputArray.clone)

    implicit def t2rt(t: (Int, Int)) = new RangedTuple(t)

    class RangedTuple(t: (Int, Int)) {
      def to(t2: (Int, Int)) = {

        def internalTo(t1: (Int, Int), t2: (Int, Int)): LazyList[(Int, Int)] = {
          (t1, t2) match {
            case ((a1: Int, a2: Int), (b1: Int, b2: Int)) => {
              if (a1 > b1) LazyList.empty
              else if (a2 > b2) internalTo((a1 + 1, t._2), (b1, b2))
              else (a1, a2) #:: internalTo((a1, a2 + 1), (b1, b2))
            }
          }
        }

        internalTo(this.t, t2)
      }
    }

    @tailrec
    def findInputs(l: LazyList[(Int, Int)]): (Int, Int) = {
      inputArray.clone match {
          case Array(a0, _, _, tail @ _*) =>
            if (runProgram(0,
                Array(a0, l.head._1, l.head._2) ++ tail
              )(0) == 19690720
            ) l.head
            else findInputs(l.tail)
      }
    }

    val part2: (Int, Int) = findInputs((0, 0) to (99, 99))

    source.close()

    println(stringOf(part1))
    println(part2)
  }
}