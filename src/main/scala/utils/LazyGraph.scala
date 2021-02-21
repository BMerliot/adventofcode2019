package utils

import scala.annotation.tailrec

trait Node[N <: Node[N]] {
  def getNeighbours: Set[N]
}

trait LazyGraph[N <: Node[N]] {
  def isDebug: Boolean = false
  def printDebug(s: String): Unit = if (isDebug) println(s)

  def startNode: N

  def stopExploration(nodes: Set[N]): Boolean

  def mergePrevious(currNodes: Set[N], exploredNodes: Set[N]): Set[N]

  @tailrec
  private def explore(nodes: Set[N], exploredNodes: Set[N]): Set[N] = {
    printDebug(
      s"""Running explore(
         |    node = $nodes
         |    exploredNodes = $exploredNodes
         |)""".stripMargin
    )
    val neighbours = nodes.flatMap(_.getNeighbours).diff(nodes)
    printDebug(s"neighbours = $neighbours")
    val mergedNodes = mergePrevious(nodes, exploredNodes)
    printDebug(s"mergedNodes = $mergedNodes")

    if (stopExploration(neighbours)) neighbours
    else explore(neighbours, mergedNodes)
  }

  lazy val explore: Set[N] = explore(Set(startNode), Set())
}
