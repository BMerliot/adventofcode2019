package utils

import org.scalatest.FunSuite

class GridTest extends FunSuite {
  test("Find") {
    val grid1 = new Grid(
      Array(
        "########################",
        "#@..............ac.GI.b#",
        "###d#e#f################",
        "###A#B#C################",
        "###g#h#i################",
        "########################"
      ).map(_.toArray)
    )
    assert(grid1.find('@') == Seq((1, 1)))

    val grid2 = new Grid(
      Array(
        "#################",
        "#i.G..c...e..H.p#",
        "########.####@###",
        "#j.A..b...f..D.o#",
        "########@########",
        "#k.E..a...g..B.n#",
        "########.########",
        "#l.F..d...h..C.m#",
        "#################"
      ).map(_.toArray)
    )
    assert(grid2.find('@') == Seq((2, 13), (4, 8)))
  }
}
