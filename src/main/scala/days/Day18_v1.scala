package days

import utils.Grid

import scala.annotation.tailrec
import scala.io.Source

object Day18_v1 {

  case class Door(name: Char) {
    def key: Key = Key(name.toLower)
  }

  case class Key(name: Char) {
    def door: Door = Door(name.toUpper)
    def toBit: Int = 1 << (name.toInt - 'a'.toInt)
  }

  case class Coordinates(x: Int, y: Int)

  object Location {
    def apply(x: Int, y: Int, c: Char): Location = {
      val k = if (c.isLower) Some(Key(c)) else None
      val d = if (c.isUpper) Some(Door(c)) else None
      Location(Coordinates(x, y), k, d)
    }
  }

  case class Location(
    coord: Coordinates,
    key: Option[Key] = None,
    door: Option[Door] = None
  )

  object Move {
    def apply(l1: Location, l2: Location, cost: Int = 1): Move = Move(Set(l1, l2), cost)
  }

  case class Move(locations: Set[Location], cost: Int) {
    assert(coord.size >= 2, s"Cannot create a move to its own location : $locations")
    assert(coord.size <= 2, s"Can only move between 2 locations : $locations")

    def coord: Set[Coordinates] = locations.map(_.coord)
    def otherLocation(coord: Coordinates): Location = locations.filter(_.coord != coord).head
  }

  case class Node(
    coord: Coordinates,
    // Since there is at most 26 keys (a to z), we can store the presence of a key on 26 bits
    // For the following int, the 1st bit correspond to a, 2nd to b...
    obtainedKeys: Int = 0
  )

  def initMovesKeysAndLocation(a: Array[Array[Char]]): (Set[Move], Set[Key], Location) = {
    val h = a.length
    val l = a(0).length
    def isPath(c: Char): Boolean = c.isLetter || c == '.' || c == '@'

    val moves = for {
      i <- 1 until h
      j <- 1 until l
    } yield {
      a(i)(j) match {
        case '#' => None
        case c if isPath(c) =>
          Seq(
            (i-1, j, a(i-1)(j)),
            (i, j-1, a(i)(j-1)),
            (i+1, j, a(i+1)(j)),
            (i, j+1, a(i)(j+1))
          )
            .filter(t => isPath(t._3))
            .map{case (i2, j2, c2) => Move(Location(i, j, c), Location(i2, j2, c2))}
      }
    }

    val keys = a.flatten.filter(_.isLower).toSet.map(Key)

    val startLocation = a
      .zipWithIndex
      .flatMap{case (line, i) => line.zipWithIndex.map{case (c, j) => (i, j, c)}}
      .filter(_._3 == '@')
      .map{case (i, j, _) => Location(Coordinates(i, j))}
      .head

    val trimmedMoves = trim(moves.flatten.toSet, startLocation)

    (trimmedMoves, keys, startLocation)
  }

  def trim(moves: Set[Move], startLocation: Location): Set[Move] = {

    val locationsWithDot = moves
      .flatMap(_.locations)
      .filterNot(l => l.key.isDefined || l.door.isDefined || l == startLocation)

    locationsWithDot
      .foldLeft(moves) { case (remainingMoves: Set[Move], location: Location) =>

        val adjacentLocations: Set[(Location, Int)] = remainingMoves
          .filter(_.locations.contains(location))
          .map(m => (m.otherLocation(location.coord), m.cost))

        val mergedMoves = (for{
          x <- adjacentLocations
          y <- adjacentLocations
          if x._1 != y._1
        } yield Move(x._1, y._1, x._2 + y._2))

        (mergedMoves ++ remainingMoves.filterNot(_.locations.contains(location)))
          .groupBy(_.locations)
          .view.mapValues(_.map(_.cost).min)
          .map{case (locations: Set[Location], cost: Int) => Move(locations, cost)}
          .toSet
      }
  }

  def breadthFirst(startLocation: Location, moves: Set[Move], allKeys: Int, mapToPrint: Array[Array[Char]]): Int = {
    var counter = 0

    @tailrec
    def step(
      nodesAndCost: Set[(Node, Int)],
      previousNodesAndCost: Set[(Node, Int)]
    ): (Set[(Node, Int)], Set[(Node, Int)]) = {

      val computeNext = (for {
        (node, cost) <- nodesAndCost
        move <- moves
        if move.coord.contains(node.coord)
      } yield {
        val nextLocation = move.otherLocation(node.coord)
        val nextKeyInt = nextLocation.key match {
          case None => 0
          case Some(k) => k.toBit
        }
        val nextNode = Node(nextLocation.coord, node.obtainedKeys | nextKeyInt)
        nextLocation.door match {
          // nextNode.obtainedKeys does not contain d.key
          case Some(d) if (nextNode.obtainedKeys & d.key.toBit) != d.key.toBit => None
          case _ => Some((nextNode, cost + move.cost))
        }
      }).flatten

      val nextPreviousNodesAndCost = (computeNext ++ previousNodesAndCost)
        .groupBy(_._1.coord)
        .view.mapValues{
          _.foldLeft(Set[(Node, Int)]()){
            case (acc: Set[(Node, Int)], (node: Node, cost: Int)) =>
              if (acc.exists(n => (n._1.obtainedKeys & node.obtainedKeys) == node.obtainedKeys && n._2 >= cost))
                acc
              else acc  ++ Set((node, cost))
          }
        }
        .values.flatten
        .toSet

      val nextNodesAndCost = nextPreviousNodesAndCost.diff(previousNodesAndCost)

      val haveAllKeys = nextNodesAndCost.map(_._1).exists(_.obtainedKeys == allKeys)

      counter += 1
      println(s"step $counter:\tnextNodes: ${nextNodesAndCost.size}\tpreviousNodes: ${previousNodesAndCost.size}")
      val grid = new Grid(
        nextNodesAndCost
          .map(_._1)
          .foldLeft(mapToPrint){
            (m: Array[Array[Char]], n: Node) =>
              val (x, y) = (n.coord.x, n.coord.y)
              m.updated(x, m(x).updated(y, '*'))
          }
      )
      grid.prettyPrint(sep = " ")

      if (haveAllKeys) (nextNodesAndCost, nextPreviousNodesAndCost)
      else if (nextNodesAndCost.isEmpty) (nextNodesAndCost, nextPreviousNodesAndCost)
      else step(nextNodesAndCost, nextPreviousNodesAndCost)
    }

    val startNode = Node(startLocation.coord)
    val (finalNodesAndCost, previousNodesAndCost) = step(Set((startNode, 0)), Set((startNode, 0)))

    finalNodesAndCost.maxBy(_._2)._2
  }

  def main(args: Array[String]): Unit = {
    val source = Source.fromFile("src/main/resources/day18.txt")
    val part1Init: Array[Array[Char]] = source.getLines().map(_.toCharArray).toArray
    source.close()

    val (moves, keys, startLocation) = initMovesKeysAndLocation(part1Init)
    val keysToBit = keys.foldLeft(0)((s, k) => s | k.toBit)
    new Grid(part1Init).prettyPrint()
    val part1Result = breadthFirst(startLocation, moves, keysToBit, part1Init)
    println(part1Result)
  }
}
