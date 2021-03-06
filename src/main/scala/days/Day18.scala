package days

import utils.{Grid, LazyGraph, Node}
import utils.TimeUtils.printElapsedTime

import scala.io.Source

object Day18 {
  def main(args: Array[String]): Unit = {
    // Read input
    val source = Source.fromFile("src/main/resources/day18.txt")
    implicit val tunnelGrid: Grid[Char] = new Grid(
      source.getLines().map(_.toCharArray).toArray
    )
    source.close()

    // Run exploration
    val explorationGraph: ExplorationGraph = new ExplorationGraph
    explorationGraph.explore
    println(explorationGraph.stepCount)
  }
}

case class Location(coord: (Int, Int), keys: Set[Char])(implicit
    grid: Grid[Char]
) extends Node[Location] {
  val (i, j) = coord

  override def getNeighbours: Set[Location] = {
    Set((i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1))
      .flatMap { c =>
        val content: Char = grid(c)
        if (Seq('.', '@').contains(content))
          Some(Location(c, keys))
        else if (content.isLower)
          Some(Location(c, keys + content))
        else if (content.isUpper && keys.contains(content.toLower))
          Some(Location(c, keys))
        else None
      }
  }

  override def equals(obj: Any): Boolean =
    obj match {
      case l: Location => l.coord == coord && l.keys == keys
      case _           => false
    }
}

class ExplorationGraph(implicit grid: Grid[Char]) extends LazyGraph[Location] {
  override val isDebug: Boolean = false
  val startTimestamp: Long = System.currentTimeMillis()

  var stepCount: Int = 0

  val startCoord: (Int, Int) = {
    val findResult = grid.find('@')
    assert(findResult.size == 1, f"Multiple entrances $findResult")
    findResult.head
  }

  override def startNode: Location = Location(startCoord, Set())

  val allKeys: Set[Char] = grid.getArray.flatten.filter(_.isLower).toSet

  override def stopExploration(locations: Set[Location]): Boolean = {
    stepCount += 1
    if (stepCount % 10 == 0) {
      println(f"$stepCount - ${locations.size}")
      printElapsedTime(startTimestamp)
    }
//    grid.prettyPrint()
    stepCount < 140 &&
    locations.exists(_.keys == allKeys)
  }

  override def mergePrevious(
      currLocations: Set[Location],
      exploredLocations: Set[Location]
  ): Set[Location] = {
    val exploredKeysByCoord: Map[(Int, Int), Set[Set[Char]]] = exploredLocations
      .groupBy(_.coord)
      .map { case (coord, locations) => coord -> locations.map(_.keys) }

    currLocations
      .groupBy(_.coord)
      .flatMap {
        case (coord, set) =>
          set
            .map(_.keys)
            .foldLeft(exploredKeysByCoord.getOrElse(coord, Set[Set[Char]]())) {
              (acc: Set[Set[Char]], first: Set[Char]) =>
                if (acc.exists(second => first.diff(second).isEmpty)) acc
                else acc.filterNot(second => second.diff(first).isEmpty) + first
            }
            .map(Location(coord, _))
      }
      .toSet
  }
}
