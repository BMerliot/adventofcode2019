import scala.annotation.tailrec

object Day12 {

  case class Dimension(position: Long, velocity: Long)

  case class Moon(dimensions: Seq[Dimension]) {
    def get(dim: Int): Dimension = dimensions(dim)

    def update(dim: Int, updatedValue: Dimension): Moon =
      Moon(dimensions.updated(dim, updatedValue))
  }

  object System {
    def update(system: System, dim: Int): System = system.update(dim)
    def update(system: System): System = system.update

    def updateStream(system: System): LazyList[System] = {
      lazy val updates: LazyList[System] = system #:: updates.map(System.update)
      updates
    }
    def updateStream(system: System, dim: Int): LazyList[System] = {
      lazy val updates: LazyList[System] = system #:: updates.map(System.update(_, dim))
      updates
    }

    def cycle(system: System): Long = {
      val cycles: Seq[Long] = for (dim <- system.moons.head.dimensions.indices)
        yield cycle(system, dim)
      cycles.foldLeft(1L)(lcm)
    }
    def cycle(system: System, dim: Int): Long = {
      @tailrec
      def cycle(stream: LazyList[System], set: Set[System], cnt: Long): Long = {
        val updatedSystem: System = stream.head
        if (set.contains(updatedSystem)) cnt
        else cycle(stream.tail, set + updatedSystem, cnt + 1)
      }

      cycle(System.updateStream(system, dim), Set(), 0)
    }
  }

  case class System(moons: List[Moon]) {
    def velocityStep(dim: Int): System = {

      val changes: List[List[Long]] = for {
        gets <- moons
      } yield for {
        gives <- moons
        if gives != gets
      } yield if (gives.get(dim).position > gets.get(dim).position) 1
              else if (gives.get(dim).position < gets.get(dim).position) -1
              else 0

      val updatedMoons: List[Moon] = moons.zip(changes)
        .map {
          case (moon: Moon, change: List[Long]) => {
            val old: Dimension = moon.get(dim)
            moon.update(dim, Dimension(old.position, old.velocity + change.sum))
          }
        }

      System(updatedMoons)
    }

    def positionStep(dim: Int): System = {

      val updatedMoons: List[Moon] = moons.map {
        moon: Moon => {
          val old: Dimension = moon.get(dim)
          moon.update(dim, Dimension(old.position + old.velocity, old.velocity))
        }
      }

      System(updatedMoons)
    }

    def update(dim: Int): System = velocityStep(dim).positionStep(dim)

    def update: System = (0 to 2).foldLeft(this)(System.update)

    def energy: Long = {

      val energies: List[Long] = for (moon <- moons)
        yield {
          val dims: Seq[Dimension] = for (dim <- 0 to 2) yield moon.get(dim)
          val (potential: Long, kynetic: Long) = dims.foldLeft((0L, 0L)) {
            (acc: (Long, Long), dim: Dimension) =>
              (acc._1 + Math.abs(dim.position), acc._2 + Math.abs(dim.velocity))
          }
          potential * kynetic
        }

      energies.sum
    }
  }

  def gcd(a: Long, b: Long): Long = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def lcm(a: Long, b: Long): Long = Math.abs((a / gcd(a, b)) * b)

  def main(args: Array[String]) = {

    val problemInput: System = System(
      Moon(Dimension(14, 0) :: Dimension(2, 0) :: Dimension(8, 0) :: Nil) ::
      Moon(Dimension(7, 0) :: Dimension(4, 0) :: Dimension(10, 0) :: Nil) ::
      Moon(Dimension(1, 0) :: Dimension(17, 0) :: Dimension(16, 0) :: Nil) ::
      Moon(Dimension(-4, 0) :: Dimension(-1, 0) :: Dimension(1, 0) :: Nil) ::
      Nil
    )

    // Test exmample
//    val problemInput: System = System(
//      Moon(Dimension(-1, 0) :: Dimension(0, 0) :: Dimension(2, 0) :: Nil) ::
//      Moon(Dimension(2, 0) :: Dimension(-10, 0) :: Dimension(-7, 0) :: Nil) ::
//      Moon(Dimension(4, 0) :: Dimension(-8, 0) :: Dimension(8, 0) :: Nil) ::
//      Moon(Dimension(3, 0) :: Dimension(5, 0) :: Dimension(-1, 0) :: Nil) ::
//      Nil
//    )

    // Works too but I wanted to try out things with LazyLists
//    val part1: System = (0 to 999).foldLeft(problemInput){
//      (system: System, _: Long) => System.update(system)
//    }

    val part1: System = System.updateStream(problemInput).take(1001).last

    println(part1.energy)

    val part2: Long = System.cycle(problemInput)

    println(part2)
  }
}
