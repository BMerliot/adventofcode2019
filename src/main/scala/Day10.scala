import scala.io.Source

object Day10 {

  def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  class Asteroid(
                val x: Int,
                val y: Int
                ) {

    def rebase(origin: Asteroid): Asteroid = new Asteroid(
      x - origin.x,
      y - origin.y
    )

    def reverseRebase(origin: Asteroid): Asteroid = new Asteroid(
      x + origin.x,
      y + origin.y
    )

    def angle(): Double = {
      math.floor(
        {
          val a: Double = if (y == 0) 0 else math.abs(x * 1.0 / y);
          val a_norm: Double = a / math.sqrt(a * a + 1);
          (x, y) match {
            case (_, 0) => if (x >= 0) math.Pi / 2 else 3 * math.Pi / 2
            case (0, _) => if (y >= 0) math.Pi else 0
            case (x, y) if x > 0 && y < 0 => math.atan(a_norm)
            case (x, y) if x < 0 && y < 0 => 2 * math.Pi - math.atan(a_norm)
            case (x, y) if x > 0 && y > 0 => math.Pi - math.atan(a_norm)
            case (x, y) if x < 0 && y > 0 => math.Pi + math.atan(a_norm)
          }
        } * 100000
      ) / 100000
    }

    def euclideanDistance(): Int = x * x + y * y

    override def toString: String = f"[$x,$y]"
  }

  def main(args: Array[String]) {

    val source = Source.fromFile("src/main/resources/day10.txt")
    val asteroids: List[Asteroid] = source.getLines
      .zipWithIndex.flatMap {
      case (s: String, y: Int) =>
        s.zipWithIndex
          .filter(_._1 == '#')
          .map { case ('#', x) => new Asteroid(x, y) }
    }.toList

    source.close()

    def countDetected(observer: Asteroid): Int = {
      asteroids
        .filter{
          case same if (same == observer) => false
          case default => true
        }
        .map {case a: Asteroid => {
          // Computing slope between each asteroid and the station
          val num: Int = a.y - observer.y
          val den: Int = a.x - observer.x
          val g: Int = gcd(num, den)
          // Normalizing the slope to make a set of (num, den). Each slope can
          // only contain 2 detected asteroids : on each side of the station.
          // The case of having one asteroid on each side is covered by dividing
          // by abs(GCD) instead of GCD. Example :
          // (-2, 4) and (2, -4) -> (-1, 2) and (1, -2)
          val norm: Int = {
            if (g == 0) {
              if (num == 0) math.abs(den)
              else math.abs(num)
            }
            else math.abs(g)
          }
          (num / norm, den / norm)
        }}.toSet.size
    }

    val detectedAsteroids: List[(Asteroid, Int)] = asteroids
      .map((a: Asteroid) => (a, countDetected(a)))

    val station: Asteroid = detectedAsteroids.maxBy(_._2)._1

    println(station)

    def destroyOrder(
                      asteroidsBySlope: List[List[Asteroid]]
                    ): List[Asteroid] = {
      asteroidsBySlope match {
        case Nil => Nil
        case Nil :: t => destroyOrder(t)
        case l :: t => l.head :: destroyOrder(t ::: (l.tail :: Nil))
      }
    }

    val asteroidsBySlope: List[List[Asteroid]] =
      asteroids
        .filter{
          case same if (same == station) => false
          case default => true
        }
        .map{ _.rebase(station) }
        .groupBy(_.angle())
        .toList
        .sortBy(_._1)
        .map(_._2.sortBy(_.euclideanDistance()))

    val part2: Asteroid = destroyOrder(asteroidsBySlope)(199)
      .reverseRebase(station)

    println(part2)
  }
}
