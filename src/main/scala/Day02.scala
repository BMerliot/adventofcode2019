import scala.annotation.tailrec
import scala.io.Source
import scala.runtime.ScalaRunTime.stringOf

object Day02 {
  def main(args: Array[String]) {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day02.txt")
    val part1Init: IntCodeMachine = new IntCodeMachine(mem)
    val part1End: IntCodeMachine = part1Init.run()
    println(part1End.mem(0))

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
      val initIter: IntCodeMachine = new IntCodeMachine(
        mem + ((BigInt(1) -> l.head._1), (BigInt(2) -> l.head._2))
      )
      val endIter: IntCodeMachine = initIter.run()
      if (endIter.mem(0).equals(BigInt(19690720))) l.head
      else findInputs(l.tail)
    }

    val part2: (Int, Int) = findInputs((0, 0) to (99, 99))
    println(part2)
  }
}
