import scala.annotation.tailrec

object Day13 {

  object Screen {
    def read(machineOutput: Seq[BigInt]): Array[Array[Int]] = {

      val formatedOutput: Seq[(Int, Int, Int)] = Screen.formatOutput(machineOutput)
      val xSize: Int = formatedOutput.map(_._1).max + 1
      val ySize: Int = formatedOutput.map(_._2).max + 1

      read(formatedOutput, Array.fill[Int](ySize, xSize)(0))
    }

    private def read(
                      formatedOutput: Seq[(Int, Int, Int)],
                      result: Array[Array[Int]]
                    ): Array[Array[Int]] = {
      formatedOutput match {
        case Nil => result
        case head :: tail => if (head._1 < 0) read(tail, result)
        else read(
            tail,
            result.updated(head._2, result(head._2).updated(head._1, head._3))
          )
      }
    }

    def readToChar(machineOutput: Seq[BigInt]): Array[Array[Char]] =
      toChars(Screen.read(machineOutput))

    def toChars(intScreen: Array[Array[Int]]): Array[Array[Char]] =
      intScreen.map{
        _.map{
          case 0 => ' '
          case 1 => '#'
          case 2 => '☐'
          case 3 => '_'
          case 4 => 'o'
        }
      }

    def formatOutput(machineOutput: Seq[BigInt]): Seq[(Int, Int, Int)] = {
      val splitByIndex: Map[Int, Seq[BigInt]] = machineOutput.zipWithIndex
        .map{case (x, i) => (x, i%3)}
        .groupBy(_._2)
        .map{case (i, t) => (i, t.map(_._1))}

      val x: Seq[Int] = splitByIndex(0).map(_.toInt)
      val y: Seq[Int] = splitByIndex(1).map(_.toInt)
      val v: Seq[Int] = splitByIndex(2).map(_.toInt)

      (x, y, v).zipped.toList
    }
  }

  def prettyPrint[T](a: Array[Array[T]]): Unit = {
    a.foreach{b: Array[T] => println(b.mkString(" "))}
  }

  class ArcadeCabinet(
                            override val mem: Map[BigInt, BigInt],
                            override val i: BigInt = 0,
                            override val machineInput: Seq[BigInt] = Nil,
                            override val machineOutput: Seq[BigInt] = Nil,
                            val xBall: BigInt = 0,
                            val xPaddle: BigInt = 0
                          )
    extends IntCodeMachine(mem, i, machineInput, machineOutput) {

    override def run(): ArcadeCabinet = runRec()

    @tailrec
    private def runRec(): ArcadeCabinet = {
      val nextArcade: ArcadeCabinet = machineOutput match {
        case x :: y :: v :: Nil => v.toInt match {
          case 3 => new ArcadeCabinet(mem, i, machineInput, Nil, xBall, x)
          case 4 => new ArcadeCabinet(mem, i, machineInput, Nil, x, xPaddle)
          case _ => {
            if (x == -1 && y == 0) println(v)
            new ArcadeCabinet(mem, i, machineInput, Nil, xBall, xPaddle)
          }
        }
        case _ => if (mem(i) == 99) this
        else next()
      }
      if (nextArcade.equals(this)) this
      else nextArcade.runRec()
    }

    override def next(): ArcadeCabinet = {
      val nextMachine: IntCodeMachine = super.next()
      new ArcadeCabinet(
        nextMachine.mem,
        nextMachine.i,
        nextMachine.machineInput,
        nextMachine.machineOutput,
        xBall,
        xPaddle
      )
    }

    override protected def readInput(): ArcadeCabinet = {
      machineInput match {
        case Nil => {
          new ArcadeCabinet(
            mem + (c -> nextCommand()), i+2, Nil, machineOutput
          )
        }
        case _ => new ArcadeCabinet(
          mem + (c -> machineInput.head), i+2, machineInput.tail, machineOutput
        )
      }
    }

    def nextCommand(): BigInt = {
      if (xBall - xPaddle > 0) 1
      else if (xBall - xPaddle < 0) -1
      else 0
    }
  }

  def main(args: Array[String]): Unit = {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day13.txt")
    val part1Init: IntCodeMachine = new IntCodeMachine(mem, machineInput = 1 :: Nil)
    val part1End: IntCodeMachine = part1Init.run()
    val part1Output: Seq[BigInt] = part1End.machineOutput
    val part1: Array[Array[Int]] = Screen.read(part1Output)

    println("Initial game screen")
    prettyPrint(Screen.readToChar(part1Output))

    print("\nPart 1 answer : ")
    println(part1.foldLeft(0){
      (acc: Int, a: Array[Int]) => acc + a.count(_ == 2)}
    )

    println("\nPart 2 scores, read last score to get the answer :")
    val part2Mem: Map[BigInt, BigInt] = mem.updated(0, 2)
    val part2Init: ArcadeCabinet = new ArcadeCabinet(part2Mem)
    val part2End: IntCodeMachine = part2Init.run()
  }
}
