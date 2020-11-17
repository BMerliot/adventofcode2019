package days

import utils.{Grid, IntCodeMachine}

import scala.annotation.tailrec
import scala.math.Ordering.Implicits._

// I just came back to this half finished day after a few months break
// I just want to get this done, so the code is messy
object Day17 {

  def continuousLadder(view: Array[String]): Array[Array[Boolean]] = {

    val view0: Array[String] = view.map { line => line + '.' + '.' }
    val view1: Array[String] = view.map { line => '.' + line + '.' }
    val view2: Array[String] = view.map { line => '.' + '.' + line }

    view0 zip view1 zip view2 map {
      case ((line0, line1), line2) => line0 zip line1 zip line2 map {
        case ((c, d), e) => c == '#' && d == '#' && e == '#'
      }
    } map (_.drop(1).dropRight(1).toArray)
  }

  def calibrate(view: Array[String]): Array[(Int, Int)] = {

    val horizontalLadder: Array[Array[Boolean]] = continuousLadder(view)
    val verticalLadder: Array[Array[Boolean]] =
      continuousLadder(
        view
          .map(_.toCharArray)
          .transpose.map(_.foldLeft("")(_ + _))
      ).transpose

    (horizontalLadder zip verticalLadder).zipWithIndex.flatMap {
      case ((line1, line2), i) => (line1 zip line2).zipWithIndex
        .filter { case ((a, b), _) => a && b }
        .map { case ((_, _), j) => (i, j) }
    }
  }

  def adjacentCells(view: Array[String]): Array[Array[Boolean]] = {

    val view0: Array[String] = view.map { line => line + '.' + '.' }
    val view1: Array[String] = view.map { line => '.' + line + '.' }
    val view2: Array[String] = view.map { line => '.' + '.' + line }

    view0 zip view1 zip view2 map {
      case ((line0, line1), line2) => line0 zip line1 zip line2 map {
        case ((c, d), e) => (c == '#' && d == '#') || (d == '#' && e == '#')
      }
    } map (_.drop(1).dropRight(1).toArray)
  }

  def getCorners(view: Array[String]): Array[(Int, Int)] = {

    val horizontalLadder: Array[Array[Boolean]] = adjacentCells(view)
    val verticalLadder: Array[Array[Boolean]] =
      adjacentCells(
        view
          .map(_.toCharArray)
          .transpose.map(_.foldLeft("")(_ + _))
      ).transpose

    (horizontalLadder zip verticalLadder).zipWithIndex.flatMap {
      case ((line1, line2), i) => (line1 zip line2).zipWithIndex
        .filter { case ((a, b), _) => a && b }
        .map { case ((_, _), j) => (i, j) }
    }
  }

  def explore(
    currentNode: (Int, Int),
    graph: Set[((Int, Int), (Int, Int))]
  ): Seq[Seq[(Int, Int)]] = {

    val possibleEdges = graph.filter {
      case (a, b) => currentNode == a || currentNode == b
    }.toSeq

    if (possibleEdges.isEmpty)
      Seq(Nil)
    else {
      possibleEdges.flatMap {
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

  def eulerianPaths(
    viewWithoutIntersections: Array[String],
    intersections: Array[(Int, Int)]
  ): Seq[Seq[(Int, Int)]] = {

    val robotSymbols: Set[Char] = Set('>', '<', '^', 'v')
    val start: (Int, Int) = viewWithoutIntersections.zipWithIndex
      .foldLeft(Nil: Seq[(Int, Int)]) {
        case (acc, (s, i)) => s.zipWithIndex
          .filter { t => robotSymbols.contains(t._1) }
          .map(t => (i, t._2)) ++ acc
      }.head
    val end: (Int, Int) = viewWithoutIntersections.zipWithIndex
      .foldLeft(Nil: Seq[(Int, Int)]) {
        case (acc, (s, i)) => s.zipWithIndex
          .filter {
            t =>
              (for {
                coord <- List((i + 1, t._2), (i - 1, t._2), (i, t._2 + 1), (i, t._2 - 1))
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
      nodes.foldLeft(viewWithoutIntersections) {
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
  }

  def translatePath(wholePath: Seq[(Int, Int)]): Seq[Char] = {
    object Direction extends Enumeration {
      type Direction = Value
      val UP, RIGHT, DOWN, LEFT = Value
    }
    import Direction._

    def buildRLformat(direction: Direction, position: (Int, Int), path: Seq[(Int, Int)]): List[Int] = {
      path match {
        case Nil => Nil
        case nextPosition :: nextPath => {
          val (str, nextDirection) = direction match {
            case UP => {
              if (position._2 > nextPosition._2) (s"L,${position._2 - nextPosition._2},", LEFT)
              else if (position._2 < nextPosition._2) (s"R,${nextPosition._2 - position._2},", RIGHT)
              else (s"${position._1 - nextPosition._1},", UP)
            }
            case RIGHT => {
              if (position._1 > nextPosition._1) (s"L,${position._1 - nextPosition._1},", UP)
              else if (position._1 < nextPosition._1) (s"R,${nextPosition._1 - position._1},", DOWN)
              else (s"${nextPosition._2 - position._2},", RIGHT)
            }
            case DOWN => {
              if (position._2 > nextPosition._2) (s"R,${position._2 - nextPosition._2},", LEFT)
              else if (position._2 < nextPosition._2) (s"L,${nextPosition._2 - position._2},", RIGHT)
              else (s"${nextPosition._1 - position._1},", DOWN)
            }
            case LEFT => {
              if (position._1 > nextPosition._1) (s"R,${position._1 - nextPosition._1},", UP)
              else if (position._1 < nextPosition._1) (s"L,${nextPosition._1 - position._1},", DOWN)
              else (s"${position._2 - nextPosition._2},", LEFT)
            }
          }
          // Not tailrec but the size of a path should be reasonable
          str.map(_.toInt).toList ::: buildRLformat(nextDirection, nextPosition, nextPath)
        }
      }
    }

    def concatGoingFoward(pathRL: List[Int]): List[Int] = pathRL match {
      case a :: 44 :: b :: 44 :: nextPathRL =>
        if ('0'.toInt <= a && a <= '9'.toInt && '0'.toInt <= b && b <= '9'.toInt)
          concatGoingFoward((a.toChar.asDigit + b.toChar.asDigit).toString.map(_.toInt).toList ::: 44 :: nextPathRL)
        else a :: 44 :: concatGoingFoward(b :: 44 :: nextPathRL)
      case a :: b :: 44 :: c :: 44 :: nextPathRL =>
        if ('0'.toInt <= c && c <= '9'.toInt)
          concatGoingFoward((a.toChar.asDigit * 10 + b.toChar.asDigit + c.toChar.asDigit).toString.map(_.toInt).toList ::: 44 :: nextPathRL)
        else a :: b :: 44 :: concatGoingFoward(c :: 44 :: nextPathRL)
      case a :: 44 :: b :: c :: 44 :: nextPathRL =>
        if ('0'.toInt <= a && a <= '9'.toInt)
          concatGoingFoward((a.toChar.asDigit + b.toChar.asDigit * 10 + c.toChar.asDigit).toString.map(_.toInt).toList ::: 44 :: nextPathRL)
        else a :: 44 :: concatGoingFoward(b :: c :: 44 :: nextPathRL)
      case a :: b :: 44 :: c :: d :: 44 :: nextPathRL =>
          concatGoingFoward((a.toChar.asDigit * 10 + b.toChar.asDigit + c.toChar.asDigit * 10 + d.toChar.asDigit).toString.map(_.toInt).toList ::: 44 :: nextPathRL)
      case _ => pathRL
    }

//    def overlap(a: Int, b: Int, c: Int, d: Int): Boolean = (a <= c && b >= c) || (a >= c && a <= d)
//
//    def bruteforceFunctions(pathRL: List[Int]): List[List[Int]] = {
//      // This is both brute force and assumes that taking successive optima will achieve a global optimum (which is
//      // not often true), but if this works I won't bother optimize (as I want a working solution, not the best one)
//      {
//        // Make all windows that could contain a function
//        for {
//          functionLength <- 2 to 10
//          offset <- 0 until pathRL.size - functionLength
//        }
//          yield (pathRL.slice(offset, offset + functionLength), offset, offset + functionLength) -> 1
//      }
//        // Group by window value (e.g. could a window appear multiple times)
//        .groupMapReduce(_._1)(_._2)(_ + _)
//        .toList
//        // Sort by the total characters the function replaces
//        .sortBy { case ((l, _, _), cnt) => l.size * cnt }
//        .map(_._1)
//        // Take the best 3 that don't overlap
//        .foldLeft(Nil: List[(List[Int], Int, Int)]) {
//          case (res, (l, start, end)) => res.size match {
//            case 0 => (l, start, end) :: res
//            case n if n >= 3 => res
//            case _ =>
//              if (res.forall { case (_, rstart, rend) => !overlap(start, end, rstart, rend) })
//                (l, start, end) :: res
//              else res
//          }
//        }
//        // Trim trailing commas
//        .map(_._1.reverse.dropWhile(_ == 44).reverse)
//    }
//
//    def replaceFunctionsInPath(path: List[Int], functions: List[List[Int]]): Option[List[Int]] = {
//      val replacedPath: List[Int] = functions
//        // Zip with ascii code of A, B, C
//        .zipWithIndex
//        .map { case (l, i) => (l, i + 'A'.toInt) }
//        .foldLeft(path) {
//          case (path: List[Int], (func: List[Int], letter: Int)) => path.foldRight(Nil: List[Int]) {
//            (i, res) =>
//              if (res.size < func.size) i :: res
//              else if (res.take(func.size) == func) i :: letter :: res.drop(func.size)
//              else i :: res
//          }
//        }
//      if (
//        replacedPath.forall(i => '0'.toInt <= i || i <= '9'.toInt) || replacedPath.size > 20
//      ) None
//      else Some(replacedPath ::: 10 :: functions.reduceLeft(_ ::: 10 :: _))
//    }
//
//    // Could not make my version work, I adapted https://github.com/lupari/aoc2019/blob/55b2cdccc465a76f489085ec71f38fe4dd5b471f/src/main/scala/challenge/Day17b.scala#L56
//    def findMovements(path: List[String]): List[List[String]] = {
//
//      @tailrec
//      def acc(xs: List[String], ms: List[List[String]]): List[List[String]] = xs match {
//        case h :: i :: j :: k :: t =>
//          val threes: List[String] = List(h, i, j)
//          val fours: List[String]  = threes :+ k
//          acc(List(i, j, k) ++ t, ms :+ threes :+ fours)
//        case h :: i :: j :: t =>
//          val threes: List[String] = List(h, i, j)
//          acc(List(i, j) ++ t, ms :+ threes)
//        case _ => ms
//      }
//
//      @tailrec
//      def removeSubList(l: List[String], sl: List[String]): List[String] = l.indexOfSlice(sl) match {
//        case -1 => l
//        case i  => removeSubList(l.patch(i, Nil, sl.length), sl)
//      }
//
//      def isCompleteMatch(path: List[String], candidate: List[List[String]]) =
//        candidate.foldLeft(path)((a, b) => removeSubList(a, b)).isEmpty
//
//      acc(path, Nil).combinations(3).find(isCompleteMatch(path, _)).get
//    }

    // Build a path like "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2,"
    val rawRLformat = buildRLformat(UP, wholePath.head, wholePath.tail).dropRight(1)
    // Concat successive integers when going forward
    val concatRLformat = concatGoingFoward(rawRLformat)
    concatRLformat.map(_.toChar)
  }

  def main(args: Array[String]): Unit = {

    val mem: Map[BigInt, BigInt] = IntCodeMachine.load("src/main/resources/day17.txt")
    val initMachine1 = new IntCodeMachine(mem, machineInput = Nil)
    val endMachine1 = initMachine1.run()

    val view: Array[String] = endMachine1.machineOutput.map(_.toChar)
      .foldLeft("")(_ + _)
      .split("\n")
    val intersections: Array[(Int, Int)] = calibrate(view)

    val part1: Int = intersections
      .map { case (i, j) => i * j }
      .sum
    println(part1)

    val paths = eulerianPaths(view, intersections)
      // For some reason the last point is not included
      .map(_ :+ (12, 4))
    val translatedPath = paths
      .map(translatePath)
      .head
      .filter(_ != ',')

    println(translatedPath)

    // It doesn't work, just hardcoding it myself
    // Writing it down from the path I printed
    // L4L8L8L8L8L4R4L4R4R2L2R4L10R2L2R2L4R6R6L12L8L8R4L10L6R2R6R2L2R6L12L8L8R4L12L4R4R2L2R4L10R2L2R2L4R6R6L12L8L8R4L12L6L2L8L6
    // Solving it by hand
    // L12L8L8L12R4L12R6L12L8L8R4L12L12R6L12R4L12R6L12L8L8R4L12L12R6L12L8L8R4L12L12R6L12L4L12R6
    // ABACBACACB
    // A = L12L8L8
    // B = L12R4L12R6
    // C = R4L12L12R6

    val mainRoutine = "A,B,A,C,B,A,C,A,C,B".toCharArray.map(_.toInt) :+ '\n'.toInt
    println(mainRoutine.mkString(","))
    val a = "L,12,L,8,L,8".toCharArray.map(_.toInt) :+ '\n'.toInt
    println(a.mkString(","))
    val b = "L,12,R,4,L,12,R,6".toCharArray.map(_.toInt) :+ '\n'.toInt
    println(b.mkString(","))
    val c = "R,4,L,12,L,12,R,6".toCharArray.map(_.toInt) :+ '\n'.toInt
    println(c.mkString(","))
    val continuous_video_feed = Array('n'.toInt, '\n'.toInt)
    println(continuous_video_feed.mkString(","))

    val initMachine2 = new IntCodeMachine(mem.updated(0, BigInt(2)), machineInput = (mainRoutine ++ a ++ b ++ c ++ continuous_video_feed).map(i => BigInt(i)))
    val part2 = initMachine2.run().machineOutput
    val part2PrettyOutput = part2.dropRight(1).map(_.toChar)
    val part2Result = part2.takeRight(1).head
    println(part2PrettyOutput.mkString)
    println(part2Result)
  }
}
