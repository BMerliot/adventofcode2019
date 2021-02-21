package days.day18

import days.ExplorationGraph
import org.scalatest.FunSuite
import utils.Grid

class Day18Test extends FunSuite {

  def runIntegrationTest(input: Array[String], expectedResult: Int): Unit = {
    implicit val grid: Grid[Char] = new Grid[Char](input.map(_.toArray))
    val explorationGraph = new ExplorationGraph
    explorationGraph.explore
    assert(explorationGraph.stepCount == expectedResult)
  }

  test("Integration test 1") {
    runIntegrationTest(
      Array(
        "#########",
        "#b.A.@.a#",
        "#########"
      ),
      8
    )
  }

  test("Integration test 2") {
    runIntegrationTest(
      Array(
        "########################",
        "#f.D.E.e.C.b.A.@.a.B.c.#",
        "######################.#",
        "#d.....................#",
        "########################"
      ),
      86
    )
  }

  test("Integration test 3") {
    runIntegrationTest(
      Array(
        "########################",
        "#...............b.C.D.f#",
        "#.######################",
        "#.....@.a.B.c.d.A.e.F.g#",
        "########################"
      ),
      132
    )
  }

  test("Integration test 4") {
    runIntegrationTest(
      Array(
        "#################",
        "#i.G..c...e..H.p#",
        "########.########",
        "#j.A..b...f..D.o#",
        "########@########",
        "#k.E..a...g..B.n#",
        "########.########",
        "#l.F..d...h..C.m#",
        "#################"
      ),
      136
    )
  }

  test("Integration test 5") {
    runIntegrationTest(
      Array(
        "########################",
        "#@..............ac.GI.b#",
        "###d#e#f################",
        "###A#B#C################",
        "###g#h#i################",
        "########################"
      ),
      81
    )
  }
}
