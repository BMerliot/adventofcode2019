package days.day18

import days.Location
import org.scalatest.FunSuite
import utils.Grid

class LocationTest extends FunSuite {
  test("getNeighbours") {
    implicit val grid: Grid[Char] = new Grid(
      Array(
        "######",
        "#a...#",
        "#.@.Q#",
        "#....#",
        "######"
      ).map(_.toArray)
    )

    val location1 = Location((2, 3), Set())
    assert(
      location1.getNeighbours == Set(
        Location((1, 3), Set()),
        Location((3, 3), Set()),
        Location((2, 2), Set())
      )
    )

    val location2 = Location((2, 3), Set('q'))
    assert(
      location2.getNeighbours == Set(
        Location((1, 3), Set('q')),
        Location((2, 4), Set('q')),
        Location((3, 3), Set('q')),
        Location((2, 2), Set('q'))
      )
    )

    val location3 = Location((1, 2), Set())
    assert(
      location3.getNeighbours == Set(
        Location((1, 1), Set('a')),
        Location((2, 2), Set()),
        Location((1, 3), Set())
      )
    )
  }
}
