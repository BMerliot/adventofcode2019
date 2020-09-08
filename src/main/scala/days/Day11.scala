package days

import utils.IntCodeMachine

object Day11 {

  object Direction extends Enumeration {

    case class Val() extends super.Val {
      def turnClockwise(): Val =
        Direction((this.id + 1) % Direction.maxId)
      def turnCounterclockwise(): Val =
        Direction((this.id - 1 + Direction.maxId) % Direction.maxId)
      def forward(coord: (Int, Int)): (Int, Int) = this match {
        case TOP => (coord._1, coord._2 + 1)
        case RIGHT => (coord._1 + 1, coord._2)
        case BOTTOM => (coord._1, coord._2 - 1)
        case LEFT => (coord._1 - 1, coord._2)
      }
    }
    implicit def valueToDirectionVal(x: Value): Val = x.asInstanceOf[Val]

    val TOP, RIGHT, BOTTOM, LEFT = Val()
  }

  case class Panel(
                    x: Int,
                    y: Int,
                    color: Int, // 0 is black, 1 is white
                    painted: Boolean = false
                  )

  implicit def intToBigInt(i: Int): BigInt = BigInt(i)
  implicit def bigIntToInt(i: BigInt): Int = i.toInt

  def main(args: Array[String]) {

    @scala.annotation.tailrec
    def runRobot(
                  coord: (Int, Int),
                  direction: Direction.Val,
                  panels: Map[(Int, Int), Panel],
                  intCodeMachine: IntCodeMachine
                ): Map[(Int, Int), Panel] = {

      intCodeMachine.machineOutput match {

        case out1 :: out2 :: outTail =>
          val paintedPanel = Panel(coord._1, coord._2, out1, painted = true)
          val nextPanels: Map[(Int, Int), Panel] = panels + (coord -> paintedPanel)
          val nextDirection: Direction.Val = out2.toInt match {
            case 0 => direction.turnCounterclockwise()
            case 1 => direction.turnClockwise()
          }
          val nextCoord: (Int, Int) = nextDirection.forward(coord)
          val nextInput: Int =
            if (nextPanels.contains(nextCoord)) nextPanels(nextCoord).color
            else 0

          runRobot(
            nextCoord,
            nextDirection,
            nextPanels,
            intCodeMachine.copy(
              machineInput = intCodeMachine.machineInput :+ nextInput,
              machineOutput = outTail
            )
          )

        case _ =>
          intCodeMachine.next() match {
            case same if same.equals(intCodeMachine) => panels
            case nextMachine => runRobot(coord, direction, panels, nextMachine)
          }
      }
    }

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day11.txt")
    val initPanels = Map[(Int, Int), Panel]() + ((0, 0) -> Panel(0, 0, 1))
    val initMachine1 = new IntCodeMachine(mem, machineInput = 0 :: Nil)
    val paintedPanels1: Map[(Int, Int), Panel] = runRobot(
      (0, 0),
      Direction.TOP,
      initPanels,
      initMachine1
    )

    val part1: Int = paintedPanels1.values
      .foldLeft(0)((n: Int, panel: Panel) => n + (if (panel.painted) 1 else 0))

    println(part1)

    val initMachine2 = new IntCodeMachine(mem, machineInput = 1 :: Nil)
    val paintedPanels2: Map[(Int, Int), Panel] = runRobot(
      (0, 0),
      Direction.TOP,
      initPanels,
      initMachine2
    )

    val dimensions: (Int, Int, Int, Int) = paintedPanels2.keys
      .foldLeft((0, 0, 0, 0)){
        case ((minX, minY, maxX, maxY), (x, y)) => (
          math.min(minX, x),
          math.min(minY, y),
          math.max(maxX, x),
          math.max(maxY, y)
        )
      }

    val part2: List[List[String]] =
      {for (y: Int <- dimensions._4 to dimensions._2 by -1)
        yield {
          for (x: Int <- dimensions._1 to dimensions._3)
            yield if (paintedPanels2.contains((x, y))
              && paintedPanels2((x, y)).color == 1) "# "
            else ". "
          }.toList
      }.toList

    part2.foreach(l => println(l.mkString("")))
  }
}
