package days

import utils.IntCodeMachine

object Day07 {
  def main(args: Array[String]) {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day07.txt")

    def allCombinations(n: BigInt, m: BigInt): List[List[BigInt]] = {

      def combinations(l: List[BigInt], remainingSwaps: BigInt): List[List[BigInt]] = {
        if (remainingSwaps == 0) l :: Nil
        else l match {
          case h :: t =>
            combinations(t, t.size - 1).map((a: List[BigInt]) => h :: a) :::
            combinations(t :+ h, remainingSwaps - 1)
          case _ => Nil
        }
      }

      combinations((n to m).toList, m - n -1)
    }

    @scala.annotation.tailrec
    def chainRunProgram(l: List[BigInt], inputInt: BigInt): BigInt = {
      l match {
        case h :: t =>
          val machineInput: List[BigInt] = h :: inputInt :: Nil
          val chainInit: IntCodeMachine = new IntCodeMachine(mem, machineInput = machineInput)
          val chainEnd: IntCodeMachine = chainInit.run()
          chainRunProgram(t, chainEnd.machineOutput.head)
        case _ => inputInt
      }
    }

    @scala.annotation.tailrec
    def loopRunProgram(
                        machines: List[IntCodeMachine],
                        lastOutput: BigInt
                      ): BigInt = {
      machines match {
        case Nil => lastOutput
        case currentMachine :: _ =>
          currentMachine.machineOutput match {
            case Nil => currentMachine.next() match {
              case same if same.equals(currentMachine) => loopRunProgram(
                machines.tail,
                lastOutput
              )
              case newMachine => loopRunProgram(
                newMachine :: machines.tail,
                lastOutput
              )
            }
            case newOutput :: remainingOutputs =>
              val nextMachine: IntCodeMachine = machines.tail.head
              val otherMachines: List[IntCodeMachine] = machines.tail.tail
              loopRunProgram(
                (new IntCodeMachine(
                  nextMachine.mem,
                  nextMachine.i,
                  nextMachine.machineInput :+ newOutput,
                  nextMachine.machineOutput
                ) :: otherMachines) ::: new IntCodeMachine(
                  currentMachine.mem,
                  currentMachine.i,
                  currentMachine.machineInput,
                  remainingOutputs
                ) :: Nil,
                newOutput
              )
          }
      }
    }

    def initInputs(l: List[BigInt]): List[List[BigInt]] = {
      l.map((i: BigInt) => i :: Nil) match {
        case h :: t => (h.head :: BigInt(0) :: Nil) :: t
      }
    }

    val part1: BigInt = allCombinations(0, 4)
      .map(chainRunProgram(_, 0))
      .max

    println(part1)

    val part2: BigInt = allCombinations(5, 9)
      .map((l: List[BigInt]) => loopRunProgram(
        initInputs(l).map((s: List[BigInt]) => new IntCodeMachine(mem, 0, s, Nil)),
        0
      ))
      .max

    println(part2)
  }
}
