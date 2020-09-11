package days

import scala.math.Ordering.Implicits._
import utils.{Grid, IntCodeMachine}

// I just came back to this half finished day and this is gonna be messy code
// I just want to get this done
object Day17 {

  def continuousLadder(view: Array[String]): Array[Array[Boolean]] = {

    val view0: Array[String] = view.map{ line => line + '.' + '.' }
    val view1: Array[String] = view.map{ line => '.' + line + '.' }
    val view2: Array[String] = view.map{ line => '.' + '.' + line }

    view0 zip view1 zip view2 map {
      case ((line0, line1), line2) => line0 zip line1 zip line2 map {
        case ((c, d), e) => c == '#' && d == '#' && e == '#'
      }
    } map ( _.drop(1).dropRight(1).toArray )
  }

  def calibrate(view: Array[String]): Array[(Int, Int)] = {

    val horizontalLadder: Array[Array[Boolean]] = continuousLadder(view)
    val verticalLadder: Array[Array[Boolean]] =
      continuousLadder(
        view
          .map(_.toCharArray)
          .transpose.map(_.foldLeft("")(_+_))
      ).transpose

    (horizontalLadder zip verticalLadder).zipWithIndex.flatMap {
      case ((line1, line2), i) => (line1 zip line2).zipWithIndex
        .filter{ case ((a, b), _) => a && b }
        .map { case ((_, _), j) => (i, j) }
    }
  }

  def adjacentCells(view: Array[String]): Array[Array[Boolean]] = {

    val view0: Array[String] = view.map{ line => line + '.' + '.' }
    val view1: Array[String] = view.map{ line => '.' + line + '.' }
    val view2: Array[String] = view.map{ line => '.' + '.' + line }

    view0 zip view1 zip view2 map {
      case ((line0, line1), line2) => line0 zip line1 zip line2 map {
        case ((c, d), e) => (c == '#' && d == '#') || (d == '#' && e == '#')
      }
    } map ( _.drop(1).dropRight(1).toArray )
  }

  def getCorners(view: Array[String]): Array[(Int, Int)] = {

    val horizontalLadder: Array[Array[Boolean]] = adjacentCells(view)
    val verticalLadder: Array[Array[Boolean]] =
      adjacentCells(
        view
          .map(_.toCharArray)
          .transpose.map(_.foldLeft("")(_+_))
      ).transpose

    (horizontalLadder zip verticalLadder).zipWithIndex.flatMap {
      case ((line1, line2), i) => (line1 zip line2).zipWithIndex
        .filter{ case ((a, b), _) => a && b }
        .map { case ((_, _), j) => (i, j) }
    }
  }

  def explore(
               currentNode: (Int, Int),
               graph: Set[((Int, Int), (Int, Int))]
             ): Seq[Seq[(Int, Int)]] = {

    val possibleEdges = graph.filter{
      case (a, b) => currentNode == a || currentNode == b
    }.toSeq

    if (possibleEdges.isEmpty) Nil
    else {
      possibleEdges.flatMap{
        edge => {
          val nextNode =
            if (edge._1 == currentNode) edge._2
            else edge._1

          explore(nextNode, graph - edge)
            .map(currentNode +: _)
        }
      }
    }
  }

  def eulerianPath(
               viewWithoutIntersections: Array[String],
               intersections: Array[(Int, Int)]
             ): Seq[Int] = {

    val robotSymbols: Set[Char] = Set('>', '<', '^', 'v')
    val start: (Int, Int) = viewWithoutIntersections.zipWithIndex
      .foldLeft(Nil: Seq[(Int, Int)]){
        case (acc, (s, i)) => s.zipWithIndex
          .filter{t => robotSymbols.contains(t._1)}
          .map(t => (i, t._2)) ++ acc
      }.head
    val end: (Int, Int) = viewWithoutIntersections.zipWithIndex
      .foldLeft(Nil: Seq[(Int, Int)]){
        case (acc, (s, i)) => s.zipWithIndex
          .filter{
            t => (for {
              coord <- List((i + 1, t._2), (i - 1, t._2), (i, t._2+1), (i, t._2-1))
              if coord._1 >= 0 && coord._1 < viewWithoutIntersections.length &&
                coord._2 >= 0 && coord._2 < viewWithoutIntersections(0).length
            } yield viewWithoutIntersections(coord._1)(coord._2))
              .count(_ != '.') == 1 &&
              viewWithoutIntersections(i)(t._2) == '#'
          }
          .map(t => (i, t._2)) ++ acc
      }.head

    val nodes: Array[(Int, Int)] = intersections :+ start :+ end :++ getCorners(viewWithoutIntersections)
    val view: Array[String] =
      nodes.foldLeft(viewWithoutIntersections){
        (v, i) => v.updated(i._1, v(i._1).updated(i._2, 'O'))
      }

    val graph: Set[((Int, Int), (Int, Int))] = (for {
      n1 <- nodes
      n2 <- nodes
      if n1 > n2 && {
        {
          n1._1 == n2._1 &&
            (for (j <- math.min(n1._2, n2._2) + 1 until math.max(n1._2, n2._2))
              yield view(n1._1)(j))
              .forall(c => c == '#')
        } || {
          n1._2 == n2._2 &&
            (for (i <- math.min(n1._1, n2._1) + 1 until math.max(n1._1, n2._1))
              yield view(i)(n1._2))
              .forall(c => c == '#')
        }
      }
    } yield (n1, n2)).toSet

    new Grid[Char](view.map(_.toCharArray)).prettyPrint()
//    graph.foreach(println)
    explore(start, graph)
      .filter(_.size == graph.size)
      .foreach(println)
    Nil
  }

  def main(args: Array[String]): Unit = {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day17.txt")
    val initMachine1 = new IntCodeMachine(mem, machineInput = Nil)
    val endMachine1 = initMachine1.run()

    val view: Array[String] = endMachine1.machineOutput.map(_.toChar)
      .foldLeft("")(_+_)
      .split("\n")
    val intersections: Array[(Int, Int)] = calibrate(view)

    val part1: Int = intersections
      .map{ case (i, j) => i * j }
      .sum
    println(part1)

    val initMachine2 = new IntCodeMachine(mem.updated(0, BigInt(2)))
    eulerianPath(view, intersections)
  }
}
