object Day09 {
  def main(args: Array[String]) {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day09.txt")
    val part1Init: IntCodeMachine = new IntCodeMachine(mem, machineInput = 2 :: Nil)
    val part1End: IntCodeMachine = part1Init.run()
    val part1: Seq[BigInt] = part1End.machineOutput
    println(part1)
  }
}
