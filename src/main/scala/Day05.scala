import scala.annotation.tailrec
import scala.io.Source

object Day05 {
  def main(args: Array[String]) {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day05.txt")
    val part1Init: IntCodeMachine = new IntCodeMachine(mem, machineInput = 1 :: Nil)
    val part1End: IntCodeMachine = part1Init.run()
    val part1: Seq[BigInt] = part1End.machineOutput
    println(part1)

    val part2Init: IntCodeMachine = new IntCodeMachine(mem, machineInput = 5 :: Nil)
    val part2End: IntCodeMachine = part2Init.run()
    val part2: Seq[BigInt] = part2End.machineOutput
    println(part2)
  }
}
