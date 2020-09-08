package days

import scala.annotation.tailrec
import scala.io.Source

object Day06 {
  def main(args: Array[String]) {

    // Order orbits so that in any orbit (parent, child), parent is either "COM"
    // or has its parent previously defined in the orbit list
    def orderOrbits(orbits: List[(String, String)]): List[(String, String)] = {

      @tailrec
      def orderOrbitsRec(orbits: List[(String, String)],
                         hasNoParent: List[(String, String)],
                         orderedOrbits: List[(String, String)]
                        ): List[(String, String)] = {
        orbits match {
          case (parent, child) :: t =>
            if ((orderedOrbits).exists(_._2 == parent) || parent.equals("COM"))
              orderOrbitsRec(t, hasNoParent, (parent, child) :: orderedOrbits)
            else orderOrbitsRec(t, (parent, child) :: hasNoParent, orderedOrbits)
          case _ => if (hasNoParent.isEmpty) orderedOrbits
            else orderOrbitsRec(hasNoParent, Nil, orderedOrbits)
        }
      }

      orderOrbitsRec(orbits, Nil, Nil).reverse
    }

    def depthMap(orderedOrbits: List[(String, String)]): Map[String, Int] = {

      @tailrec
      def depthMapRec(orbits: List[(String, String)],
                      dm: Map[String, Int]): Map[String, Int] = {
        orbits match {
          case (parent, child) :: t =>
            // dm(parent) should not raise an exception if the orbits were
            // correctly ordered before being passed to depthMap
            depthMapRec(t, dm + (child -> (dm(parent) + 1)))
          case _ => dm
        }
      }

      depthMapRec(orderedOrbits, Map[String, Int]("COM" -> 0))
    }

    val source = Source.fromFile("src/main/resources/day06.txt")
    val lines = source.getLines

    val orderedOrbits: List[(String, String)] = orderOrbits(
      lines.map(_.split(')') match {case Array(a, b) => (a, b)}).toList)
    val dm: Map[String, Int] = depthMap(orderedOrbits)
    val part1: Int = dm.foldLeft(0)((n: Int, t: (String, Int)) => n + t._2)

    def breadthFirst(orbits: List[(String, String)]): Int = {

      @tailrec
      def breadthFirstRec(o: List[(String, String)],
                          curr: List[String],
                          next: List[String],
                          steps: Int): Int = {
        o match {
          case (a, b) :: t  if (curr.contains(a)) =>
            if (b.equals("SAN")) steps + 1
            else breadthFirstRec(t, curr, b :: next, steps)
          case (a, b) :: t if (curr.contains(b)) =>
            if (a.equals("SAN")) steps + 1
            else breadthFirstRec(t, curr, a :: next, steps)
          case _ :: t => breadthFirstRec(t, curr, next, steps)
          case _ => breadthFirstRec(orbits, next, Nil, steps + 1)
        }
      }

      breadthFirstRec(orbits, "YOU" :: Nil, Nil, 0) - 2
    }

    val part2: Int = breadthFirst(orderedOrbits)

    println(part1)
    println(part2)

    source.close()
  }
}
