package days.day18

import days.{ExplorationGraph, Location}
import org.scalatest.FunSuite
import utils.Grid

class ExplorationGraphTest extends FunSuite {
  test("startnode") {
    implicit val grid: Grid[Char] = new Grid(
      Array(
        "######",
        "#a...#",
        "#.@..#",
        "#....#",
        "######"
      ).map(_.toArray)
    )
    val explorationGraph = new ExplorationGraph
    assert(explorationGraph.startNode == Location((2, 2), Set()))
  }

  test("stopExploration") {
    implicit val grid: Grid[Char] =
      new Grid(Array(Array('a', '.', '#', '.', '@', 'b')))
    val explorationGraph = new ExplorationGraph

    assert(explorationGraph.allKeys == Set('a', 'b'))

    assert(
      explorationGraph.stopExploration(
        Set(
          Location((0, 0), Set('a')),
          Location((0, 0), Set()),
          Location((0, 0), Set('a', 'b'))
        )
      )
    )
    assert(explorationGraph.stepCount == 1)

    assert(
      !explorationGraph.stopExploration(
        Set(
          Location((0, 0), Set('a')),
          Location((0, 0), Set('b'))
        )
      )
    )
    assert(explorationGraph.stepCount == 2)
  }

  test("mergePrevious") {
    implicit val grid: Grid[Char] =
      new Grid(Array(Array('a', '.', '#', '.', '@', 'b')))
    val explorationGraph = new ExplorationGraph

    assert(
      explorationGraph.mergePrevious(
        Set(
          Location((0, 0), Set('a', 'b')),
          Location((0, 5), Set('b')),
          Location((0, 2), Set('a'))
        ),
        Set(
          Location((0, 0), Set('a')),
          Location((0, 5), Set('a', 'b')),
          Location((0, 2), Set())
        )
      ) == Set(
        Location((0, 0), Set('a', 'b')),
        Location((0, 5), Set('a', 'b')),
        Location((0, 2), Set('a'))
      )
    )

    assert(
      explorationGraph.mergePrevious(
        Set(
          Location((1, 17), Set('a')),
          Location((1, 15), Set()),
          Location((1, 13), Set())
        ),
        Set(
          Location((1, 15), Set()),
          Location((1, 16), Set()),
          Location((1, 14), Set())
        )
      ) == Set(
        Location((1, 13), Set()),
        Location((1, 14), Set()),
        Location((1, 15), Set()),
        Location((1, 16), Set()),
        Location((1, 17), Set('a'))
      )
    )

    assert(
      explorationGraph.mergePrevious(
        Set(
          Location((0, 0), Set('a', 'b')),
          Location((0, 0), Set('c'))
        ),
        Set(
          Location((0, 0), Set('a')),
          Location((0, 0), Set('a', 'c'))
        )
      ) == Set(
        Location((0, 0), Set('a', 'b')),
        Location((0, 0), Set('a', 'c'))
      )
    )
  }
}
