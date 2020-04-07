import scala.annotation.tailrec

object Day15 {

  def flightDistance(t1: (Int, Int), t2: (Int, Int)): Int =
    math.abs(t1._1 - t2._1) + math.abs(t1._2 - t2._2)

  object ExplorationRobot {

    def positionToCommand(robotPosition: (Int, Int)): Map[(Int, Int), Int] = Map(
      (robotPosition._1, robotPosition._2 + 1) -> 1,
      (robotPosition._1, robotPosition._2 - 1) -> 2,
      (robotPosition._1 + 1, robotPosition._2) -> 3,
      (robotPosition._1 - 1, robotPosition._2) -> 4
    )
  }

  class ExplorationRobot(
                               override val mem: Map[BigInt, BigInt],
                               override val i: BigInt = 0,
                               override val machineInput: Seq[BigInt] = Nil,
                               override val machineOutput: Seq[BigInt] = Nil,
                               envMap: Map[(Int, Int), Int] = Map((0, 0) -> 3),
                               lastMove: Int = 1
                        )
    extends IntCodeMachine(mem, i, machineInput, machineOutput) {

    lazy val robotPosition: (Int, Int) =
      envMap.filter(_._2.equals(3)).keys.head

    lazy val positionToCommand: Map[(Int, Int), Int] =
      ExplorationRobot.positionToCommand(robotPosition)

    override def next(): ExplorationRobot = {
      val nextMachine: IntCodeMachine = super.next()
      new ExplorationRobot(
        nextMachine.mem,
        nextMachine.i,
        nextMachine.machineInput,
        nextMachine.machineOutput,
        envMap,
        lastMove
      )
    }

    @tailrec
    private def runUntilInput(): ExplorationRobot = {
      (mem(i) % 100).toInt match {
        case 3 => this
        case _ => next().runUntilInput()
      }
    }

    private def adjacentCoord(coord: (Int, Int)): Seq[(Int, Int)] =
      coord match {
        case (x, y) => (x, y+1) :: (x+1, y) :: (x, y-1) :: (x-1, y) :: Nil
      }

    @tailrec
    final def breadthFirst(
                              f: (((Int, Int), Int)) => Boolean,
                              paths: Seq[Seq[(Int, Int)]] = Nil,
                              visited: Set[(Int, Int)] = Set.empty,
                              startPosition: (Int, Int) = robotPosition
                            ): Seq[(Int, Int)] = {

      if (paths.isEmpty) {

        val startPaths: Seq[Seq[(Int, Int)]] = adjacentCoord(startPosition)
          .filterNot(envMap.getOrElse(_, -1) == 0)
          .map(_ :: Nil)

        val startVisited: Set[(Int, Int)] = visited ++ startPaths.flatten.toSet

        breadthFirst(f, startPaths, startVisited)

      } else {

        val pathsWithEnvMapValue: Seq[Seq[((Int, Int), Int)]] =
          paths.map(_.map{coord => (coord, envMap.getOrElse(coord, -1))})

        if (pathsWithEnvMapValue.map(_.last).exists(f))
          pathsWithEnvMapValue.filter{
            s => f(s.last)
          }
            .head.map(_._1)

        else {

          val updatedPaths: Seq[Seq[(Int, Int)]] = paths.flatMap{
            path => for {
              nextCoord <- adjacentCoord(path.last)
              if envMap.getOrElse(nextCoord, -1) != 0 &&
                !visited.contains(nextCoord)
            } yield path :+ nextCoord
          }

          val updatedVisited: Set[(Int, Int)] =
            visited ++ updatedPaths.map(_.last)

          breadthFirst(f, updatedPaths, updatedVisited)
        }
      }
    }

    private def chooseNextMove(): Int = {

      val path: Seq[(Int, Int)] = breadthFirst{
        case (coord, _) => !envMap.contains(coord)
      }

      path match {
        case Nil => positionToCommand(
          adjacentCoord(robotPosition)
            .filter(envMap.contains)
            .head
        )
        case h :: _ => positionToCommand(h)
      }
    }

    @tailrec
    final def explore(): ExplorationRobot = {

      machineOutput match {
        case Nil => {
          val nextMove: Int = chooseNextMove()
          new ExplorationRobot(
            mem, i, nextMove :: Nil, Nil, envMap, nextMove
          )
            .runUntilInput()
            .next()
            .runUntilInput()
            .explore()
        }
        case h :: t => {

          val locationToUpdate: (Int, Int) = positionToCommand.map{
            case (k, v) => (v, k)
          }(lastMove)

          val updatedEnvMap: Map[(Int, Int), Int] = h.toInt match {
            case 0 => envMap.updated(locationToUpdate, 0)
            case 1 => envMap.updated(locationToUpdate, 3)
              .updated(robotPosition, 1)
            case 2 => envMap.updated(locationToUpdate, 2)
          }

          val updatedExplorationRobot: ExplorationRobot =
            new ExplorationRobot(
              mem, i, Nil, Nil, updatedEnvMap, lastMove
            )

          h.toInt match {
            case 2 => updatedExplorationRobot
            case _ => updatedExplorationRobot.explore()
          }
        }
      }
    }

    def getEnvMap: Map[(Int, Int), Int] = envMap
  }

  def dijkstra(
                envMap: Map[(Int, Int), Int],
                dijkstraMap: Map[(Int, Int), Int] = Map((0, 0) -> 0)
              ): Map[(Int, Int), Int] = {

    if (dijkstraMap.size == envMap.values.filterNot(_ == 0).size)
      dijkstraMap
    else {

      val updatedDijkstraMap: Map[(Int, Int), Int] = dijkstraMap.flatMap{
        case (coord, dist) => ExplorationRobot.positionToCommand(coord)
          .keys
          .filter(envMap.getOrElse(_, 0) != 0)
          .map((_, dist + 1))
      } ++ dijkstraMap

      dijkstra(envMap, updatedDijkstraMap)
    }
  }

  def prettyPrint(envMap: Map[(Int, Int), Int]): Unit = {

    val maxX: Int = envMap.foldLeft(0){(acc, v) => math.max(acc, v._1._1.toInt)}
    val maxY: Int = envMap.foldLeft(0){(acc, v) => math.max(acc, v._1._2.toInt)}
    val minX: Int = envMap.foldLeft(0){(acc, v) => math.min(acc, v._1._1.toInt)}
    val minY: Int = envMap.foldLeft(0){(acc, v) => math.min(acc, v._1._2.toInt)}

    val grid: Seq[Seq[String]] =
      for (y <- minY to maxY + 1)
        yield for (x <- minX to maxX + 1)
        yield envMap.get((x, y)) match {
          case None => " "
          case Some(0) => "#"
          case Some(1) => "."
          case Some(2) => "G"
          case Some(3) => "D"
        }

    grid.updated(-minY, grid(-minY).updated(-minX, "0"))
      .foreach(a => println(a.mkString("")))
  }

  def main(args: Array[String]): Unit = {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day15.txt")
    val initRobot: ExplorationRobot = new ExplorationRobot(mem)
    val endRobot: ExplorationRobot = initRobot.explore()
    val exploredEnvironment: Map[(Int, Int), Int] = endRobot.getEnvMap

    prettyPrint(exploredEnvironment)

    val dijkstraMap: Map[(Int, Int), Int] = dijkstra(exploredEnvironment)
    val oxygenCoord: (Int, Int) =
      exploredEnvironment.filter(_._2 == 2).keys.head
    val part1: Int = dijkstraMap.filter{
      t => t._1 == oxygenCoord
    }.values.head

    println(part1)

    val part2: Int = dijkstra(exploredEnvironment, Map(oxygenCoord -> 0))
      .values.max

    println(part2)
  }
}
